import { loadRuntime } from "./jazz_wasm";

type FinalizationData = { ptr: number; drop: (ptr: number) => void };

const HEAP: any[] = [null, document.body];
const FREE_HEAP: number[] = [];
const DECODER = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
const ENCODER = new TextEncoder();
const FINALIZER = new FinalizationRegistry(({ drop, ptr }: FinalizationData) => drop(ptr));

export const PROPS: { [key: number]: { component: any, arr: any[], f: () => void } } = {};

(window as any).HEAP = HEAP;

interface Abi {
  memory: WebAssembly.Memory;
  abi_buffer__drop(ptr: number): void;
  abi_buffer__len(ptr: number): number;
  abi_buffer__new(len: number): number;
  abi_buffer__ptr(ptr: number): number;
  abi_callback__call(ptr: number): void;
  abi_callback__drop(ptr: number): void;
  abi_event_callback__call(callbackPtr: number, target_ptr: number): void;
  abi_event_callback__drop(callbackPtr: number): void;
  run(): void;
}

let ABI: Abi = null as unknown as Abi;

function addToHeap(item: any) {
  let ptr = FREE_HEAP.pop();
  if(ptr === undefined) {
    ptr = HEAP.length;
    HEAP.push(item);
  } else {
    HEAP[ptr] = item;
  }
  return ptr;
}
function dropFromHeap(ptr: number) {
  let result = HEAP[ptr];
  if(ptr > 2) {
    delete HEAP[ptr];
    FREE_HEAP.push(ptr);
  }
  return result;
}
function getHeapObject(ptr: number) {
  return HEAP[ptr];
}


class AbiBuffer {
  static fromRuntimePtr(ptr: number, finalize = true) {
    return new AbiBuffer(ptr, undefined, finalize);
  }
  static fromRuntimePtrToString(ptr: number) {
    const buf = new AbiBuffer(ptr, undefined, false);
    const string = buf.toString();
    ABI.abi_buffer__drop(buf.ptr);
    return string;
  }
  static fromString(string: string) {
    return new AbiBuffer(undefined, string);
  }

  readonly ptr: number;
  readonly len: number;
  private buf: number;

  constructor(ptr: number|undefined, string?: string, finalize = true) {
    if(ptr) {
      this.ptr = ptr;
      this.buf = ABI.abi_buffer__ptr(this.ptr);
      this.len = ABI.abi_buffer__len(this.ptr);
    } else {
      const encoded = ENCODER.encode(string);
      this.ptr = ABI.abi_buffer__new(encoded.length);
      this.buf = ABI.abi_buffer__ptr(this.ptr);
      this.len = ABI.abi_buffer__len(this.ptr);

      const buffer = new Uint8Array(ABI.memory.buffer).subarray(this.buf, this.buf + encoded.length);
      buffer.set(encoded);
    }
    if(finalize) {
      FINALIZER.register(this, {ptr: this.ptr, drop: ABI.abi_buffer__drop});
    }
  }
  toString() {
    if(this.len === 0) {
      return '';
    }
    const buffer = new Uint8Array(ABI.memory.buffer).subarray(this.buf, this.buf + this.len);
    return DECODER.decode(buffer);
  }
  indexUsize(i: number): number | undefined {
    if(i > Math.floor(this.len / 4)) {
      return undefined;
    }
    const bufptr = Math.floor(this.buf / 4);
    const len = Math.floor(this.len / 4);
    const buffer = new Uint32Array(ABI.memory.buffer).subarray(bufptr, bufptr + len);
    return buffer.at(i);
  }
}

// class AbiResult {
//   static new() {
//     return new AbiResult();
//   }

//   readonly ptr: number;

//   constructor() {
//     this.ptr =  ABI.abi_result__new();
//     FINALIZER.register(this, {ptr: this.ptr, drop: ABI.abi_result__drop});
//   }
//   isOk() {
//     return !!ABI.abi_result__is_ok(this.ptr);
//   }
//   isErr() {
//     return !!ABI.abi_result__is_err(this.ptr);
//   }
//   get message() {
//     return AbiBuffer.fromRuntimePtrToString(ABI.abi_result__message(this.ptr));
//   }
//   verify() {
//     if(this.isErr()) {
//       throw new Error(this.message);
//     }
//   }
// }

(async () => {
  const wasm_imports = {
    runtime: {
      __dispatch_function(ptr: number, args: number) {
        const fn = getHeapObject(ptr);
        if(fn instanceof Function) {
          return fn(args);
        } else {
          console.error('tried to dispatch non-function!', ptr, fn);
          return 0;
        }
      },
      __drop_function(ptr: number) {
        dropFromHeap(ptr);
      },
      __console_log(ptr: number, isError: 0|1) {
        const message = AbiBuffer.fromRuntimePtrToString(ptr);
        if(isError) {
          console.error(message);
        } else {
          console.log(message);
        }
      },
      __document_create_text_node(ptr: number) {
        const textContent = AbiBuffer.fromRuntimePtrToString(ptr);
        return addToHeap(document.createTextNode(textContent));
      },
      __document_create_element(ptr: number) {
        const tag = AbiBuffer.fromRuntimePtrToString(ptr);
        return addToHeap(document.createElement(tag));
      },
      __request_animation_frame(ptr: number) {
        const callback = () => ABI.abi_callback__call(ptr);
        return requestAnimationFrame(callback);
      },
      __html_node_remove(thisPtr: number) {
        const e: ChildNode = getHeapObject(thisPtr);
        e.remove();
      },
      __html_element_contains(thisPtr: number, otherPtr: number) {
        const e: ChildNode = getHeapObject(thisPtr);
        const other: ChildNode = getHeapObject(otherPtr);
        return e.contains(other);
      },
      __html_insert_before(thisPtr: number, otherPtr: number, siblingPtr: number) {
        const e: ChildNode = getHeapObject(thisPtr);
        const other: ChildNode = getHeapObject(otherPtr);
        const sibling: ChildNode = getHeapObject(siblingPtr);
        e.insertBefore(other, sibling);
      },
      __html_text_set_text_content(thisPtr: number, textContentPtr: number) {
        const e: Text = getHeapObject(thisPtr);
        const textContent = AbiBuffer.fromRuntimePtrToString(textContentPtr);
        e.textContent = textContent;
      },
      __html_add_event_listener(thisPtr: number, eventNamePtr: number, callbackPtr: number) {
        const eventName = AbiBuffer.fromRuntimePtrToString(eventNamePtr);
        const callback = (event: Event) => ABI.abi_event_callback__call(callbackPtr, addToHeap(event.target));
        const e = getHeapObject(thisPtr);
        e.addEventListener(eventName, callback);
      },
      __html_get_value(thisPtr: number) {
        const e = getHeapObject(thisPtr);
        return AbiBuffer.fromString(e.value).ptr;
      },
      // __random() {
      //   return Math.random();
      // },
      // __get_current_timestamp() {
      //   return Date.now();
      // },
    },
  };
  const WASM = await loadRuntime(wasm_imports);
  ABI = WASM.instance.exports as unknown as Abi;
  ABI.run();
})();

import jazz, {
  Component,
  ComponentConfig,
  State,
  StyleMap,
  System,
  // ConcreteHtmlComponent,
  // Foreach,
  // HtmlComponent,
  // HtmlInputComponent,
  // TextComponent
} from "@dragonpop/jazz";
// import { FlashCards } from "./app/app";

// export type Card = { front: string, back: string };
// export type Deck = { name: string, cards: Card[] };

// import SpanishAnimalsDeck from "./decks/spanish/animals";
// import SpanishBodyDeck from "./decks/spanish/body";
// import SpanishBasicsDeck from "./decks/spanish/basics";
// import SpanishBusinessDeck from "./decks/spanish/business";
// import SpanishCityDeck from "./decks/spanish/city";
// import SpanishDevicesDeck from "./decks/spanish/devices";
// import SpanishFoodDeck from "./decks/spanish/food";
// import SpanishHouseDeck from "./decks/spanish/house";
// import SpanishTransportationDeck from "./decks/spanish/transportation";

// const DECKS: Deck[] = [
//   { name: "test", cards: [{ front: "a", back: "A" }, { front: "b", back: "B" }, { front: "c", back: "C" }] },
//   SpanishAnimalsDeck,
//   SpanishBodyDeck,
//   SpanishBasicsDeck,
//   SpanishBusinessDeck,
//   SpanishCityDeck,
//   SpanishDevicesDeck,
//   SpanishFoodDeck,
//   SpanishHouseDeck,
//   SpanishTransportationDeck,
// ];

// function loadStoredDecks() {
//   const storedDecksJson = localStorage.getItem("savedDecks");
//   let storedDecks;
//   try {
//     storedDecks = JSON.parse(storedDecksJson ?? "") ?? [];
//   } catch {
//     storedDecks = [];
//   }
//   return storedDecks;
// }

// function saveDeck(name: string, cards: Card[]) {
//   const storedDecks = loadStoredDecks();
//   storedDecks.push({ name, cards });
//   localStorage.setItem("savedDecks", JSON.stringify(storedDecks));
// }

// function removeDeck(i: number) {
//   const storedDecks = loadStoredDecks();
//   storedDecks.splice(i, 1);
//   localStorage.setItem("savedDecks", JSON.stringify(storedDecks));
// }

// type ChooseDeckProps = { deckIndex: number | null, chooseDeck: ((deck: Deck) => void) };
// type ChooseDeckState = {
//   storedDecks: Deck[];
//   staticDecks: Deck[];
//   removeDeckClicked: ($state: State<ChooseDeckState>, i: number) => void;
//   chooseDeckClicked: (props: ChooseDeckProps, e: Event, deck: Deck) => void;
//   aStyle: StyleMap;
//   buttonStyle: StyleMap;
// };

// const ChooseDeck: ComponentConfig<ChooseDeckProps, ChooseDeckState> = {
//   name: "ChooseDeck",
//   props: { deckIndex: null, chooseDeck: () => null },
//   state: {
//     storedDecks: [],
//     staticDecks: DECKS,
//     removeDeckClicked: ($state: State<ChooseDeckState>, i: number) => {
//       removeDeck(i);
//       $state.set.storedDecks(loadStoredDecks());
//     },
//     chooseDeckClicked: ($props: ChooseDeckProps, e: Event, deck: Deck) => {
//       e.preventDefault();
//       $props.chooseDeck!(deck);
//     },
//     aStyle: { color: "#5959af" },
//     buttonStyle: {
//       color: "red",
//       background: "transparent",
//       border: "none",
//       cursor: "pointer",
//       fontSize: "20px",
//     },
//   },
//   onInserted(component) {
//     const { deckIndex, chooseDeck } = component.props.value;
//     const storedDecks = loadStoredDecks();
//     component.state.set.storedDecks(storedDecks);
//     if (deckIndex !== null && deckIndex >= 0) {
//       if (deckIndex < DECKS.length) {
//         chooseDeck(DECKS[deckIndex]);
//       } else if (deckIndex - DECKS.length < storedDecks.length) {
//         chooseDeck(storedDecks[deckIndex - DECKS.length]);
//       }
//     }
//   },
//   template: /*jsx*/`
//     <table>
//       @foreach {deck in staticDecks} {
//         <tr>
//           <td><button style={{...buttonStyle, visibility: "hidden"}}>&times;</button></td>
//           <td><a href="" style={aStyle} events={{click: e => chooseDeckClicked($props, e, deck)}}>{deck.name}</a></td>
//         </tr>
//       }
//       @foreach {deck, i in storedDecks} {
//         <tr>
//           <td><button style={buttonStyle} events={{click: () => removeDeckClicked($state, i)}}>&times;</button></td>
//           <td><a href="" style={aStyle} events={{click: e => chooseDeckClicked($props, e, deck)}}>{deck.name}</a></td>
//         </tr>
//       }
//     </table>
//   `,
// };

type AppState = {
//   deckIndex: number | null;
//   deck: Deck | null;
//   saveDeck: (name: string, cards: Card[]) => void;
};

// const App: ComponentConfig<{}, AppState> = {
//   name: "App",
//   props: {},
//   state: { /*deckIndex: null, deck: null, saveDeck*/ },
//   // imports: { ChooseDeck: () => System.createComponent(ChooseDeck), FlashCards: () => System.createComponent(FlashCards) },
//   // template: /*jsx*/`
//   //   @if {deck === null} {
//   //     <ChooseDeck deckIndex={deckIndex} chooseDeck={deck => $state.set.deck(deck)}/>
//   //   } @else {
//   //     <FlashCards
//   //       cards={deck.cards}
//   //       name={deck.name}
//   //       chooseDeck={() => $state.set.deck(null)}
//   //       saveDeck={saveDeck}
//   //     />
//   //   }
//   // `,
//   template: /*jsx*/`
//     <span>Hello, World!!! 🥳🥳🥳</span>
//   `,
//   // render() {
//   //   const input = Component.newHtmlInputComponent();
//   //   const text = Component.newTextComponent("hello, world!");
//   //   text.bind(input, (props, state) => ({ textContent: state.value.value }))
//   //   const span = Component.newHtmlComponent("span");
//   //   span.children = [input, text];
//   //   return [span];
//   // }
// };

// const app = System.createComponent(App);
// (window as any).App = app;
// jazz.mountComponent(document.body, (window as any).App);
// console.log(app)
