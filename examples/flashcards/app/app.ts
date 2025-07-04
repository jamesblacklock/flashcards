import jazz, { Component, ComponentConfig, State, System } from "@dragonpop/jazz";
import { Card } from "../index";

type SaveAnswersProps = {
  cards: Card[];
  name: string;
  saveDeck: ((name: string, cards: Card[]) => void);
  close: (() => void);
}

type SaveAnswersState = {
  inputValue: string;
  save: ($props: SaveAnswersProps, $state: State<SaveAnswersState>) => void;
};

const SaveAnswers: ComponentConfig<SaveAnswersProps, SaveAnswersState> = {
  name: "SaveAnswers",
  props: { cards: [], name: "", saveDeck: () => null, close: () => null },
  state: {
    inputValue: "",
    save: ($props: SaveAnswersProps, $state: State<SaveAnswersState>) => {
      $props.saveDeck!($state.value.inputValue, $props.cards);
      $props.close!();
    },
  },
  onInserted(component) {
    const d = new Date;
    const month = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "November", "December"][d.getMonth()];
    component.state.set.inputValue(`${component.props.value.name} – Missed (${month} ${d.getDate()})`);
  },
  template: /*jsx*/`
    <div
      style={{
        position: "fixed",
        left: "0",
        right: "0",
        top: "0",
        bottom: "0",
        background: "#0005",
        display: "flex",
        alignItems: "center",
      }}
    >
      <div style={{ margin: "auto", padding: "24px", background: "white" }}>
        <h3>Save Missed Answers</h3>
        // {createPortal("test", document.body)}
        <div>
          <input
            style={{width: "300px"}}
            value={inputValue}
            events={{ change: e => $state.set.inputValue(e.target.value) }}
          />
        </div>
        <div
          style={{
            display: "flex",
            justifyContent: "flex-end",
            gap: "10px",
            paddingTop: "24px",
          }}
        >
          <button events={{ click: close }}>Cancel</button>
          <button events={{ click: () => save($props, $state) }}>Save</button>
        </div>
      </div>
    </div>
  `,
}

type FlashCardsProps = {
  cards: Card[];
  name: string;
  chooseDeck: (() => void);
  saveDeck: ((name: string, cards: Card[]) => void);
}

type FlashCardsState = {
  nextRoundCards: Card[];
  currentCards: Card[];
  phase: "front"|"back"|"done";
  index: number;
  showSaveModal: boolean;
  reset: ($props: FlashCardsProps, $state: State<FlashCardsState>, startOver?: boolean) => () => void;
  back: ($state: State<FlashCardsState>) => void;
  next: ($state: State<FlashCardsState>, incorrect?: boolean) => () => void;
};

export const FlashCards: ComponentConfig<FlashCardsProps, FlashCardsState> = {
  name: "FlashCards",
  props: { cards: [], name: "", chooseDeck: () => null, saveDeck: () => null },
  state: {
    nextRoundCards: [],
    currentCards: [],
    phase: "front",
    index: 0,
    showSaveModal: false,
    reset: ($props: FlashCardsProps, $state: State<FlashCardsState>, startOver?: boolean) => () => {
      const nextCards = (startOver || $state.value.nextRoundCards.length < 1) ? [...$props.cards] : $state.value.nextRoundCards;
      $state.set.currentCards(nextCards.sort(() => Math.random() - 0.5));
      $state.set.nextRoundCards([]);
      $state.set.phase("front");
      $state.set.index(0);
    },
    back: ($state: State<FlashCardsState>, ) => {
      if ($state.value.phase === "back") {
        $state.set.phase("front");
        return;
      }
      $state.set.index($state.value.index - 1);
      if ($state.value.index > 0
        && $state.value.nextRoundCards.length > 0
        && $state.value.nextRoundCards[$state.value.nextRoundCards.length - 1].front === $state.value.currentCards[$state.value.index - 1].front
      ) {
        $state.value.nextRoundCards.pop();
        $state.set.nextRoundCards([...$state.value.nextRoundCards]/*, { dirty: true }*/);
      }
    },
    next: ($state: State<FlashCardsState>, incorrect?: boolean) => () => {
      if ($state.value.phase === "front") {
        $state.set.phase("back");
        return;
      }
      if (incorrect) {
        $state.value.nextRoundCards.push($state.value.currentCards[$state.value.index]);
        $state.set.nextRoundCards([...$state.value.nextRoundCards]/*, { dirty: true }*/);
      }
      if ($state.value.index + 1 < $state.value.currentCards.length) {
        $state.set.phase("front");
        $state.set.index($state.value.index + 1);
      } else {
        $state.set.phase("done");
      }
    },
  },
  onInserted(component) {
    component.state.value.reset(component.props.value, component.state)();
  },
  imports: { SaveAnswers: () => System.createComponent(SaveAnswers) },
  template: /*jsx*/`
    <button events={{click: chooseDeck}}>Choose Deck</button>
    <button events={{click: () => $state.set.showSaveModal(true)}}>Save Missed Answers</button>
    <h2>{name}</h2>
    <div>{phase === "done" ? "Round over" : ${"`${index + 1} / ${cards.length} cards`"}}</div>
    <div
      style={{
        border: "1px solid #aaa",
        borderRadius: "8px",
        width: "3in",
        height: "2in",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      {phase === "done" ? ${"`You missed ${nextRoundCards.length}`"} : cards[index]?.[phase] ?? ""}
    </div>
    <div
      style={{
        display: "flex",
        alignItems: "center",
        gap: "4px",
      }}
    >
      @if {phase === "front"} {
        @if {index > 0} {
          <button events={{click: () => back($state)}}>&lt;</button>
        }
        <button events={{click: next($state)}}>Flip Card</button>
      } @elseif {phase === "back"} {
        <button events={{click: () => back($state)}}>&lt;</button>
        <button events={{click: next($state)}}>Correct</button>
        <button events={{click: next($state, true)}}>Incorrect</button>
      } @else {
        <button events={{click: reset($props, $state, true)}}>Start Over</button>
        @if {nextRoundCards.length > 0} {
          <button events={{click: reset($props, $state)}}>Keep Going</button>
        }
      }
    </div>
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        gap: "12px",
      }}
    >
      <table>
        @foreach {e in nextRoundCards} {
          <tr>
            <td style={{padding: "0 12px"}}>{e.front}</td>
            <td style={{padding: "0 12px"}}>{e.back}</td>
          </tr>
        }
      </table>
    </div>
    @if {showSaveModal} {
      <SaveAnswers
        name={name}
        cards={nextRoundCards}
        saveDeck={saveDeck}
        close={() => $state.set.showSaveModal(false)}
      />
    }
  `,
}
