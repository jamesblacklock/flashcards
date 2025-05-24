declare global {
  namespace JSX {
    interface IntrinsicAttributes {
      key?: any;
    }
    interface IntrinsicElements {
      [key: string]: IntrinsicAttributes & HtmlComponentProps<any>;
    }
  }
}

type SetterOptions = { dirty?: boolean, quiet?: boolean };
type Setter<T> = (value: T, options?: SetterOptions) => void

export class State {
  component?: Component;
  target?: HTMLElement;
  dirty: boolean = false;
  private state: Record<keyof any, any> = {};
  private effectCleanup: Record<keyof any, () => void> = {};
  private deps: Record<keyof any, any[]|undefined> = {};
  private nextState: Record<keyof any, any> = {};
  private quiet: Set<string> = new Set;
  private frame: number = 0;
  private renderOptions: RenderOptions;
  private node?: VNode | null;

  constructor(node: VNode, renderOptions: RenderOptions) {
    this.node = node;
    this.renderOptions = renderOptions ?? {};
  }
  use<T>(name: string, value: T | (() => T), deps?: any[]): [T, Setter<T>] {
    return this.useInternal(name, value, deps, true, false);
  }
  useCallback<T>(name: string, callback: (() => T), deps: any[]): T {
    return this.useInternal(name, callback, deps, false, false)[0];
  }
  useEffect(name: string, effect: (() => (() => void) | void), deps: any[]): void {
    this.useInternal(name, effect, deps, true, true);
  }
  cleanup() {
    for (const effect in this.effectCleanup) {
      this.effectCleanup[effect]();
    }
  }
  private useInternal<T>(name: string, value: T | (() => T), deps: any[] | undefined, call: boolean, effect: boolean): [T, Setter<T>] {
    const depsChanged = this.depsChanged(name, deps);
    if (!(name in this.state) || depsChanged) {
      if (effect) {
        this.effectCleanup[name]?.();
      }
      if (call && value instanceof Function) {
        value = value();
      }
      if (effect && value instanceof Function) {
        this.effectCleanup[name] = value;
      }
      this.state[name] = value;
      this.deps[name] = deps;
    }
    if (depsChanged) {
      this.set(name, value, { dirty: true });
    }
    return [this.state[name], this.set.bind(this, name)];
  }
  private set(name: string, value: any, options?: SetterOptions) {
    const nans = Number.isNaN(value) && Number.isNaN(this.state[name]);
    if (!nans && value !== this.state[name] || options?.dirty) {
      if (options?.quiet) {
        this.quiet.add(name);
      } else {
        this.quiet.delete(name);
      }
      this.nextState[name] = value;
      this.render();
    } else {
      delete this.nextState[name];
    }
  }
  private depsChanged(name: string, deps?: any[]) {
    if (deps?.length !== this.deps[name]?.length) {
      return true;
    }
    for (let i=0; i<(deps?.length ?? 0); i++) {
      if (deps?.[i] !== this.deps[name]?.[i]) {
        return true;
      }
    }
    return false;
  }
  private render() {
    cancelAnimationFrame(this.frame);
    this.frame = requestAnimationFrame(() => {
      let quiet = true;
      for (const key in this.nextState) {
        this.state[key] = this.nextState[key];
        quiet = quiet && this.quiet.has(key);
      }
      this.nextState = {};
      this.quiet = new Set;
      if (!quiet) {
        this.dirty = true;
        renderInternal({r: Symbol(), options: this.renderOptions}, this.node!, this.component!);
        updateHtml(this.node!);
      }
    });
  }
}

export type DebugInfo = { renderCount: number, debug?: boolean };
export type Props = Record<keyof any, any>;
export type UIElement<T extends Props = Props> = {
  type: ComponentFunction<T> | string;
  props?: T;
  content?: UINode;
  key?: any;
};
export type UINode = UINode[] | UIElement | UIElement[] | string | number | false | null | undefined;

function isElement(a: any): a is UIElement {
  return "type" in a && (a.type instanceof Function || typeof a.type === "string")
}

function uiNodeToComponentArray(uiNode: UINode): Component[] {
  if (Array.isArray(uiNode)) {
    const components = [];
    for (const n of uiNode) {
      components.push(...uiNodeToComponentArray(n));
    }
    return components;
  }
  if (uiNode == false || uiNode === null || uiNode === undefined) {
    return [];
  }
  if (typeof uiNode === "string" || typeof uiNode === "number") {
    return [{ type: "TEXT", props: { textContent: uiNode.toString() } }];
  }
  if (isElement(uiNode)) {
    return [{ ...uiNode, content: uiNodeToComponentArray(uiNode.content) }];
  }
  console.warn("invalid UINode", uiNode);
  return [];
}

export type ComponentFunction<T extends Props = Props> = (props: T, state: State, debugInfo: DebugInfo) => UINode;

export type Component<T extends Props = Props> = {
  type: ComponentFunction<T> | string;
  props?: T;
  content?: Component[];
  key?: any;
};

type VNode = {
  r?: Symbol;
  cc: number;
  component: Component;
  nodes: VNode[];
  domNode?: Text|HTMLElement;
  domParent: HTMLElement;
  state?: State;
};

const DOM_ROOTS = new Map<Node, VNode>;

function propsNotEqual(l: Props | undefined, r: Props | undefined, depth: number) {
  if (l === r) {
    return false;
  }
  const propNames = Array.from(new Set([...Reflect.ownKeys(l ?? {}), ...Reflect.ownKeys(r ?? {})]));
  let i = 0;
  const limit = 20;
  for (const k of propNames) {
    if (i++ >= limit) {
      return true;
    } else if (depth > 0 && l?.[k]?.constructor === Object && l?.[k]?.constructor === Object) {
      if (propsNotEqual(l?.[k], r?.[k], depth-1)) {
        return true;
      }
    } else if (l?.[k] !== r?.[k]) {
      return true;
    }
  }
  return false;
}

function contentNotEqual(l?: Component, r?: Component) {
  if (l?.type !== r?.type) {
    return true;
  } else if (propsNotEqual(l?.props, r?.props, 1)) {
    return true;
  } else if (l?.key !== r?.key) {
    return true;
  }
  const lContent = l?.content && (Array.isArray(l?.content) ? l.content : []);
  const rContent = r?.content && (Array.isArray(r?.content) ? r.content : []);
  if (lContent?.length !== rContent?.length) {
    return true;
  } else if (lContent?.length) {
    for (let i=0; i<lContent.length; i++) {
      if (contentNotEqual(lContent[i], rContent![i])) {
        return true;
      }
    }
  }
  return false;
}

function nodeChanged(node: VNode, component: Component) {
  if (node.cc === 0 || contentNotEqual(node.component, component)) {
    node.cc++;
    return true;
  }
  return false;
}

function getNode(parentNode: VNode, component: Component, renderedNodes: VNode[]): VNode {
  let node = parentNode.nodes.find(node =>
    node.component.key === component.key && node.component.type === component.type && node.r !== parentNode.r
  );
  if (node) {
    node.r = parentNode.r;
  } else {
    node = {
      r: parentNode.r,
      cc: 0,
      component: { type: component.type, key: component.key },
      nodes: [],
      domParent: (parentNode.domNode ?? parentNode.domParent) as HTMLElement,
    };
  }
  renderedNodes.push(node);
  return node;
}

function* reversed<T>(arr: T[]): Generator<T> {
  for (let i=arr.length-1; i>=0; --i) {
    yield arr[i];
  }
}

function updateHtml(node: VNode, nextSibling?: Node | null) {
  const contained = node.domParent.contains(node.domNode ?? null);
  const siblingMatches = node.domNode?.nextSibling === nextSibling;
  if (node.domNode && !(contained && siblingMatches)) {
    node.domParent.insertBefore(node.domNode, nextSibling ?? null);
  }
  let lastInserted: Node | null = null;
  for (const childNode of reversed(node.nodes)) {
    lastInserted = updateHtml(childNode, lastInserted) ?? lastInserted;
  }

  return node.domNode;
}

function removeNode(node: VNode) {
  node.state?.cleanup();
  if (node.domNode) {
    node.domNode.remove();
    return;
  }
  node.nodes.forEach(removeNode);
}

type RenderState = {
  r: Symbol;
  options: RenderOptions;
}

function renderInternal(state: RenderState, node: VNode, component: Component) {
  node.r = state.r;
  const renderedNodes: VNode[] = [];
  if (component.type === "TEXT") {
    const textContent: string = (component.props?.textContent ?? "").toString();
    if (!node.domNode) {
      node.domNode = document.createTextNode(textContent);
    }
    if (textContent !== node.domNode.textContent) {
      node.domNode.textContent = textContent;
    }
  } else {
    if (typeof component.type === "string" && !node.domNode) {
      node.domNode = document.createElement(component.type);
    }
    if (!node.state) {
      node.state = new State(node, state.options);
      node.state!.component = component;
    }
    if (node.state!.dirty || nodeChanged(node, component)) {
      node.state!.dirty = false;
      let componentFactory: ComponentFunction = typeof component.type === "string"
        ? htmlComponent.bind(null, node.domNode as HTMLElement) as ComponentFunction
        : component.type;
      const renderedContent = componentFactory(
        {content: component.content, ...component.props},
        node.state!,
        { debug: state.options?.debug, renderCount: node.cc }
      );
      component.content = uiNodeToComponentArray(renderedContent);
      for (const childComponent of component.content) {
        const childNode = getNode(node, childComponent, renderedNodes);
        if (nodeChanged(childNode, childComponent)) {
          renderInternal(state, childNode, childComponent);
        }
      }
    }
  }

  for (const n of node.nodes) {
    if (n.r !== node.r) {
      removeNode(n);
    }
  }

  node.nodes = renderedNodes;
  node.component = component;
}

renderUI.fragment = () => null as unknown as Component[];
renderUI.createElement = function<P extends {}>(
  type: string | ComponentFunction,
  props: P & { key?: any } | null, 
  ...content: UINode[]
): UINode {
  const { key, ...restProps } = props ?? {};
  if (type === renderUI.fragment) {
    return content;
  }
  return { type, key, props: restProps, content };
}

export type RenderOptions = {
  debug?: boolean;
};

export default function renderUI(domTarget: HTMLElement, component: Component, options: RenderOptions = {}) {
  let node = DOM_ROOTS.get(domTarget);
  if (!node) {
    node = { cc: 0, component: { type: component.type, key: component.key }, nodes: [], domParent: domTarget };
    DOM_ROOTS.set(domTarget, node);
  }
  node.component = { type: component.type, key: component.key };
  renderInternal({r: Symbol(), options}, node, component);
  updateHtml(node);
}

export type RefObject<T = any> = { current?: T };
export type RefFunction<T = any> = ((current: T) => void)
export type Ref<T = any> = RefObject<T> | RefFunction<T>;
type EventsMap = Partial<Record<keyof HTMLElementEventMap, EventListenerOrEventListenerObject>>;
type StyleMap = Partial<Record<keyof CSSStyleDeclaration, string | number>>;
type HtmlComponentProps<T extends keyof HTMLElementTagNameMap> = {
  class?: string;
  className?: string;
  events?: EventsMap;
  style?: StyleMap;
  id?: string;
  ref?: Ref<HTMLElementTagNameMap[T]>;
  content?: UINode;
};
function htmlComponent<T extends keyof HTMLElementTagNameMap>(
  e: HTMLElementTagNameMap[T],
  props: HtmlComponentProps<T>,
  state: State,
  debugInfo: DebugInfo,
) {
  const { events = {}, style, id, ref, className } = props;
  const [attachedEvents] = state.use<EventsMap>("events", {});
  const [savedStyle, setSavedStyle] = state.use<StyleMap | undefined>("style", undefined);
  if (id) {
    e.id = id;
  } else {
    e.removeAttribute("id");
  }
  if (props.class ?? className) {
    e.className = props.class ?? className ?? "";
  } else {
    e.removeAttribute("class");
  }
  const eventNames = Array.from(new Set([...Object.keys(events), ...Object.keys(attachedEvents)])) as (keyof EventsMap)[];
  for (const event of eventNames) {
    if(events[event] !== attachedEvents[event]) {
      if (attachedEvents[event]) {
        e.removeEventListener(event, attachedEvents[event]);
        delete attachedEvents[event];
      }
      if(events[event]) {
        e.addEventListener(event, events[event]);
        attachedEvents[event] = events[event];
      }
    }
  }
  if (propsNotEqual(style, savedStyle, 0)) {
    setSavedStyle(style, { quiet: true });
    e.removeAttribute("style");
    for (const prop in style) {
      if (style[prop as any]) {
        e.style[prop as any] = style[prop as any]!.toString();
      }
    }
  }
  if (ref) {
    if (ref instanceof Function) {
      ref(e);
    } else {
      ref.current = e;
    }
  }

  if (debugInfo.debug) {
    e.setAttribute("data-render-count", debugInfo.renderCount.toString());
  }

  return props.content;
}
