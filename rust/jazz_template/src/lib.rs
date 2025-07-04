extern crate proc_macro;
extern crate proc_macro2;
use std::{collections::{HashMap, VecDeque}};

use proc_macro2::Literal;
use proc_macro::{token_stream::IntoIter, Delimiter, TokenStream, TokenTree};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{Expr, Stmt};

#[proc_macro]
pub fn jazz_template(tokens: TokenStream) -> TokenStream {
  let tokens = Tokens::new(tokens.into_iter());
  let (ast, _) = parse_template(tokens, false);

  let mut stream = quote!(let mut children = Vec::new(););
  for node in ast {
    let result = compile_node(node);
    stream = quote!(#stream #result);
  }
  stream = quote! {
    #[allow(unused_braces, unused_parens)]
    fn render(component: &Component<Self::Props, Self::State>) -> Vec<AnyComponent> {
      #stream
      children
    }
  };
  TokenStream::from(stream)
}

fn compile_node(node: AstNode) -> proc_macro2::TokenStream {
  match node {
    AstNode::Element(Element { tag, props, ast }) => {
      let bind = if props.len() > 0 {
        let mut bindings = quote!();
        for (k, v) in props {
          let k = format_ident!("{}", k);
          let is_closure = match &v {
            PropValue::Expr(syn::Expr::Block(block)) => {
              match block.block.stmts.last() {
                Some(Stmt::Expr(Expr::Closure(_), None)) => true,
                _ => false
              }
            },
            _ => false,
          };
          if is_closure {
            bindings = quote! {
              #bindings
              props.#k = callback!({props, state} #v);
            };
          } else {
            bindings = quote! {
              #bindings
              props.#k = #v;
            };
          }
        }

        let config = if tag == "input" {
          quote!(HtmlInput)
        } else {
          quote!(Html)
        };
        quote! {
          child.bind(&component, move |props, state, ()| {
            let mut props: <#config as ComponentConfig>::Props = Default::default();
            #bindings
            Some(props)
          });
        }
      } else {
        quote!()
      };
      let children = if ast.len() > 0 {
        let mut children = quote! {
          let mut children = Vec::new();
        };
        for node in ast {
          let result = compile_node(node);
          children = quote!(#children #result);
        }
        quote! {
          #children
          child.set_children(children);
        }
      } else {
        quote!()
      };
      let config = if tag == "input" {
        quote!(HtmlInput)
      } else {
        quote!(Html(#tag))
      };
      quote! {{
        let child = component.system().create_html_component(#config);
        children.push(child.any());
        #bind
        #children
      }}
    },
    AstNode::Expr(expr) => {
      quote! {{
        let child = component.system().create_text_component("");
        children.push(child.any());
        child.bind(&component, move |props, state, ()| {
          Some(HtmlTextProps {
            text_content: #expr
          })
        });
      }}
    },
  }
}

struct Tokens {
  it: IntoIter,
  peeked: VecDeque<TokenTree>,
}

impl Iterator for Tokens {
  type Item = TokenTree;
  fn next(&mut self) -> Option<Self::Item> {
    if self.peeked.len() > 0 {
      self.peeked.pop_front()
    } else {
      self.it.next()
    }
  }
}

impl Tokens {
  fn new(it: IntoIter) -> Self {
    Tokens { it, peeked: VecDeque::new() }
  }
  fn peek(&mut self, n: usize) -> Option<&TokenTree> {
    while self.peeked.len() <= n {
      if let Some(next) = self.it.next() {
        self.peeked.push_back(next);
      } else {
        break;
      }
    }
    self.peeked.get(n)
  }
}

#[derive(Debug)]
enum PropValue {
  String(String),
  Expr(syn::Expr),
}

impl ToTokens for PropValue {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match &self {
      PropValue::Expr(expr) => {
        expr.to_tokens(tokens);
      },
      PropValue::String(string) => {
        tokens.append(Literal::string(string));
      },
    }
  }
}

#[derive(Debug)]
struct Element {
  tag: String,
  props: HashMap<String, PropValue>,
  ast: Vec<AstNode>,
}

#[derive(Debug)]
enum AstNode {
  Element(Element),
  Expr(syn::Expr),
}

fn is_char(t: Option<&TokenTree>, c: char) -> bool {
  match t {
    Some(TokenTree::Punct(punct)) => punct.as_char() == c,
    _ => false,
  }
}

fn ident(t: Option<&TokenTree>) -> Option<String> {
  match t {
    Some(TokenTree::Ident(ident)) => ident.span().source_text(),
    _ => None,
  }
}

fn string(t: Option<&TokenTree>) -> Option<String> {
  match t {
    Some(TokenTree::Literal(lit)) => {
      let str = format!("{}", lit);
      if str.chars().next() == Some('"') {
        return Some(str);
      }
      None
    },
    _ => None,
  }
}

fn block(t: Option<&TokenTree>) -> Option<TokenStream> {
  match t {
    Some(TokenTree::Group(group)) => {
      if group.delimiter() == Delimiter::Brace {
        return Some(TokenStream::from(t.unwrap().clone()));
      }
      None
    },
    _ => None,
  }
}

fn parse_template(mut tokens: Tokens, in_element: bool) -> (Vec<AstNode>, Tokens) {
  let mut ast: Vec<AstNode> = Vec::new();
  while tokens.peek(0).is_some() {
    let t1_block = block(tokens.peek(0));
    let t1_is_langle = is_char(tokens.peek(0), '<');
    let t1_is_slash = is_char(tokens.peek(0), '/');
    let t2_is_slash = is_char(tokens.peek(1), '/');

    if t1_is_langle && t2_is_slash { // || is_char(t1, '}')
      if !in_element {
        panic!("syntax error: unexpected closing tag: {}", tokens.peek(0).unwrap());
      }
      break;
    }
    if t1_is_slash && t2_is_slash {
      unimplemented!("parse comment {:?}", tokens.peek(0));
    } else if t1_is_langle {
      let result;
      (result, tokens) = parse_element(tokens);
      ast.push(result);
    } else if let Some(block) = t1_block {
      tokens.next();
      // if (remaining.match(/^\{[\s\n]*@/)) {
      //   [result, remaining] = parseDirective(remaining);
      // } else {
        let expr: syn::Expr = syn::parse(block).expect("syntax error");
      // }
      ast.push(AstNode::Expr(expr));
    } else if is_char(tokens.peek(0), '@') {
      let result;
      (result, tokens) = parse_directive(tokens);
      ast.push(result);
    } else {
      let result;
      (result, tokens) = parse_text(tokens);
      if let Some(result) = result {
        ast.push(result);
      }
    }
  }
  return (ast, tokens);
}

fn parse_props(mut tokens: Tokens) -> (HashMap<String, PropValue>, Tokens) {
  let mut props: HashMap<String, PropValue> = HashMap::new();
  loop {
    let t = tokens.peek(0);
    if t.is_none() || is_char(t, '/') || is_char(t, '>') {
      break;
    }
    let name = ident(tokens.next().as_ref()).expect("syntax error: expected property name");
    if !is_char(tokens.next().as_ref(), '=') {
      panic!("syntax error: expected '=': {:?}", tokens.next());
    }
    let t = tokens.next();
    if let Some(value) = string(t.as_ref()) {
      props.insert(name, PropValue::String(value));
    } else if let Some(block) = block(t.as_ref()) {
      let expr: syn::Expr = syn::parse(block).expect("syntax error");
      props.insert(name, PropValue::Expr(expr));
    } else {
      panic!("syntax error: expected property value: {:?}", tokens.next());
    }
  }
  return (props, tokens);
}

fn parse_element(mut tokens: Tokens) -> (AstNode, Tokens) {
  let tag = { tokens.next(); tokens.next() };
  let tag = if let Some(tag) = ident(tag.as_ref()) {
    tag
  } else {
    panic!("syntax error: expected tag: {:?}", tag);
  };

  // if (tag[0].toLowerCase() === tag[0] && !(tag in TAGS!)) {
  //   throw new Error("syntax error");
  // }

  let props;
  (props, tokens) = parse_props(tokens);
  let mut self_close = false;
  if is_char(tokens.peek(0), '/') {
    self_close = true;
    let t = {tokens.next(); tokens.next() };
    if !is_char(t.as_ref(), '>') {
      panic!("syntax error: expected '>': {:?}", t);
    }
  } else {
    if !is_char(tokens.next().as_ref(), '>') {
      panic!("syntax error: expected '>': {:?}", tokens.peek(0));
    }
  }
  let mut ast: Vec<AstNode> = Vec::new();
  if !self_close {
    (ast, tokens) = parse_template(tokens, true);
    if !is_char(tokens.next().as_ref(), '<') || !is_char(tokens.next().as_ref(), '/') {
      panic!("syntax error: expected closing tag for \"{}\": {:?}", tag, tokens.peek(0));
    }
    if ident(tokens.next().as_ref()).as_ref() != Some(&tag) {
      panic!("syntax error: expected closing tag for \"{}\": {:?}", tag, tokens.peek(0));
    }
    if !is_char(tokens.next().as_ref(), '>') {
      panic!("syntax error: expected closing tag for \"{}\": {:?}", tag, tokens.peek(0));
    }
  }

  (AstNode::Element(Element { tag, ast, props }), tokens)
  // return [{ type: "element", tag, ast, props }, remaining] as const;
}

fn parse_directive(mut tokens: Tokens) -> (AstNode, Tokens) {
  unimplemented!("parse directive {:?}", tokens.peek(0));
  // let braceEnclosed = remaining[0] === '{';
  // let full, directive;
  // if (braceEnclosed) {
  //   full = remaining.match(/^\{[\s\n]*/)![0];
  //   remaining = remaining.slice(full.length);
  // }

  // [full, directive] = remaining.match(/^@(\w+)\b[\s\n]*/) ?? [];
  // if (!full) {
  //   throw new Error("syntax error");
  // }
  // remaining = remaining.slice(full.length);

  // let result: Directive;
  // if (directive === "children") {
  //   result = { type: "directive", directive: "children" };
  // } else {
  //   if (remaining[0] !== '{') {
  //     throw new Error("syntax error");
  //   }

  //   if (directive === "if" || (allowElseIf && directive === "elseif")) {
  //     let script;
  //     [script, remaining] = parseScript(remaining);
  //     full = remaining.match(/^[\s\n]*\{/)?.[0];
  //     if (!full) {
  //       throw new Error("syntax error");
  //     }

  //     let ast;
  //     [ast, remaining] = parseTemplate(remaining.slice(full.length), true);
  //     if (remaining[0] !== '}') {
  //       throw new Error("syntax error");
  //     }

  //     let elseAst;
  //     full = remaining.match(/^}[\s\n]*(?=@elseif\b)/)?.[0];
  //     if (full) {
  //       remaining = remaining.slice(full.length);
  //       let elseIf;
  //       [elseIf, remaining] = parseDirective(remaining, true);
  //       elseAst = [elseIf];
  //     } else {
  //       full = remaining.match(/^\}[\s\n]*@else[\s\n]*{/)?.[0];
  //       if (full) {
  //         [elseAst, remaining] = parseTemplate(remaining.slice(full.length), true);
  //         if (remaining[0] !== '}') {
  //           throw new Error("syntax error");
  //         }
  //       }
  //       remaining = remaining.slice(1);
  //     }

  //     result = { type: "directive", directive: "if", expr: script.expr, ast, elseAst };
  //   } else if (directive === "foreach") {
  //     let itemName, indexName;
  //     [full, itemName, indexName] = remaining.match(/^\{[\s\n]*(\w+)(?:[\s\n]*,[\s\n]*(\w+))?[\s\n]+in[\s\n]+/) ?? [];
  //     if (!full) {
  //       throw new Error("syntax error");
  //     }
  //     let script;
  //     [script, remaining] = parseScript(remaining.slice(full.length), 0);
  //     full = remaining.match(/^[\s\n]*\{/)?.[0];
  //     if (!full) {
  //       throw new Error("syntax error");
  //     }

  //     let ast;
  //     [ast, remaining] = parseTemplate(remaining.slice(full.length), true);
  //     if (remaining[0] !== '}') {
  //       throw new Error("syntax error");
  //     }

  //     remaining = remaining.slice(1);
  //     result = { type: "directive", directive: "foreach", itemName, indexName, expr: script.expr, ast };
  //   } else {
  //     throw new Error("syntax error");
  //   }
  // }

  // if (braceEnclosed) {
  //   full = remaining.match(/^[\s\n]*\}/)?.[0];
  //   if (!full) {
  //     throw new Error("syntax error");
  //   }
  //   remaining = remaining.slice(full.length);
  // }

  // return [result, remaining];

}

fn parse_text(mut tokens: Tokens) -> (Option<AstNode>, Tokens) {
  unimplemented!("parse text {:?}", tokens.peek(0));
  // let offset = 0;
  // while (remaining[offset] && !remaining.slice(offset, offset + 2).match(/^(?:[<\{\}@]|\/\/)/)) {
  //   offset++;
  // }
  // const match = remaining.slice(0, offset);
  // const entitiesSplit = match.replaceAll(/[\s\n]+/g, " ").split(/(?<=&\w+;)|(?=&\w+;)/);
  // let result = "";
  // for (let piece of entitiesSplit) {
  //   if (piece.match(/^&\w+;$/)) {
  //     piece = ENTITIES[piece as keyof typeof ENTITIES] ?? piece;
  //   }
  //   result += piece;
  // }
  // return [result === " " ? "" : result, remaining.slice(match.length)];

}

