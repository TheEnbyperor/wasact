#![feature(proc_macro_span)]
extern crate proc_macro;

use proc_macro::{token_stream, TokenStream, TokenTree};
use std::iter::FromIterator;

use proc_macro_hack::proc_macro_hack;

#[derive(Debug)]
struct ElementProp {
    name: String,
    value: proc_macro::TokenTree,
    is_function: bool
}

#[derive(Debug)]
struct TagElement {
    tag: String,
    props: Vec<ElementProp>,
    children: Vec<Element>,
}

#[derive(Debug)]
struct StringElement(String);

#[derive(Debug)]
enum Element {
    TagElement(TagElement),
    StringElement(StringElement),
    RustElement(proc_macro::TokenStream),
}

fn expect_token(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<proc_macro::TokenTree> {
    if let Some(t) = input.next() {
        Ok(t)
    } else {
        Err(failure::format_err!("Expected token"))
    }
}

fn expect_punct(input: &mut std::iter::Peekable<token_stream::IntoIter>, val: char) -> failure::Fallible<proc_macro::Punct> {
    if let proc_macro::TokenTree::Punct(t) = expect_token(input)? {
        if t.as_char() == val {
            Ok(t)
        } else {
            Err(failure::format_err!("Expected {} token, found {}", val, t.as_char()))
        }
    } else {
        Err(failure::format_err!("Expected {} token", val))
    }
}

fn expect_ident(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<proc_macro::Ident> {
    if let proc_macro::TokenTree::Ident(t) = expect_token(input)? {
        Ok(t)
    } else {
        Err(failure::format_err!("Expected identifier"))
    }
}

fn expect_literal(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<proc_macro::Literal> {
    if let proc_macro::TokenTree::Literal(t) = expect_token(input)? {
        Ok(t)
    } else {
        Err(failure::format_err!("Expected literal value"))
    }
}

fn expect_group(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<proc_macro::Group> {
    if let proc_macro::TokenTree::Group(t) = expect_token(input)? {
        Ok(t)
    } else {
        Err(failure::format_err!("Expected literal value"))
    }
}

impl ElementProp {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        let name = expect_ident(input)?;
        expect_punct(input, '=')?;
        let mut is_function = false;

        let value = match expect_token(input)? {
            proc_macro::TokenTree::Literal(l) => l.into(),
            proc_macro::TokenTree::Group(g) => g.into(),
            proc_macro::TokenTree::Ident(i) => i.into(),
            proc_macro::TokenTree::Punct(p) => {
                if p.as_char() == '|' {
                    expect_punct(input, '|')?;
                    is_function = true;
                    expect_group(input)?.into()
                } else {
                    failure::bail!("Invalid value for element property")
                }
            }
        };

        Ok(Self {
            name: format!("{}", name),
            value,
            is_function
        })
    }

    fn into_tokens(self) -> TokenStream {
        let value: TokenTree = match self.is_function {
            true => match self.value {
                proc_macro::TokenTree::Group(g) => {
                    proc_macro::Group::new(
                        proc_macro::Delimiter::Parenthesis,
                        TokenStream::from_iter::<Vec<TokenTree>>(vec![
                            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("std", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("boxed", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("Box", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("new", proc_macro::Span::call_site()).into(),
                            proc_macro::Group::new(
                                proc_macro::Delimiter::Parenthesis,
                                TokenStream::from_iter::<Vec<TokenTree>>(vec![
                                    proc_macro::Ident::new("move", proc_macro::Span::call_site()).into(),
                                    proc_macro::Punct::new('|', proc_macro::Spacing::Alone).into(),
                                    proc_macro::Punct::new('|', proc_macro::Spacing::Alone).into(),
                                    g.into()
                                ]),
                            ).into(),
                            proc_macro::Ident::new("as", proc_macro::Span::call_site()).into(),
                            proc_macro::Ident::new("Box", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new('<', proc_macro::Spacing::Joint).into(),
                            proc_macro::Ident::new("dyn", proc_macro::Span::call_site()).into(),
                            proc_macro::Ident::new("Fn", proc_macro::Span::call_site()).into(),
                            proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::new()).into(),
                            proc_macro::Punct::new('>', proc_macro::Spacing::Joint).into(),
                        ]),
                    ).into()
                },
                _ => unreachable!()
            }
            false => self.value
        };

        TokenStream::from_iter::<Vec<TokenTree>>(vec![
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("wasact", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("ElementProp", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("new", proc_macro::Span::call_site()).into(),
            proc_macro::Group::new(
                proc_macro::Delimiter::Parenthesis,
                TokenStream::from_iter::<Vec<TokenTree>>(vec![
                    proc_macro::Literal::string(&self.name).into(),
                    proc_macro::Punct::new(',', proc_macro::Spacing::Alone).into(),
                    value
                ]),
            ).into()
        ])
    }
}

impl TagElement {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        expect_punct(input, '<')?;
        let tag = expect_ident(input)?;

        let mut props: Vec<ElementProp> = vec![];

        loop {
            let mut fork = input.clone();
            match ElementProp::parse(&mut fork) {
                Ok(e) => {
                    props.push(e);
                    *input = fork;
                }
                Err(_) => break
            }
        }

        expect_punct(input, '>')?;

        let mut children: Vec<Element> = vec![];

        loop {
            let mut fork = input.clone();
            match Element::parse(&mut fork) {
                Ok(e) => {
                    children.push(e);
                    *input = fork;
                }
                Err(_) => break
            }
        }

        expect_punct(input, '<')?;
        expect_punct(input, '/')?;
        let tag2 = expect_ident(input)?;

        if tag.to_string() != tag2.to_string() {
            return Err(failure::format_err!("unmatched open and closing tags"));
        }

        expect_punct(input, '>')?;

        Ok(Self {
            tag: format!("{}", tag),
            props,
            children,
        })
    }

    fn into_tokens(self) -> TokenStream {
        TokenStream::from_iter::<Vec<TokenTree>>(vec![
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("wasact", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("Element", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("Tag", proc_macro::Span::call_site()).into(),
            proc_macro::Group::new(
                proc_macro::Delimiter::Parenthesis,
                TokenStream::from_iter::<Vec<TokenTree>>(vec![
                    proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                    proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                    proc_macro::Ident::new("wasact", proc_macro::Span::call_site()).into(),
                    proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                    proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                    proc_macro::Ident::new("TagElement", proc_macro::Span::call_site()).into(),
                    proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
                    proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
                    proc_macro::Ident::new("new", proc_macro::Span::call_site()).into(),
                    proc_macro::Group::new(
                        proc_macro::Delimiter::Parenthesis,
                        TokenStream::from_iter::<Vec<TokenTree>>(vec![
                            proc_macro::Literal::string(&self.tag).into(),
                            proc_macro::Punct::new(',', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("vec", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new('!', proc_macro::Spacing::Alone).into(),
                            proc_macro::Group::new(
                                proc_macro::Delimiter::Bracket,
                                TokenStream::from_iter::<Vec<TokenTree>>(
                                    self.props.into_iter().map(|p| vec![
                                        p.into_tokens().into_iter(),
                                        TokenStream::from_iter::<Vec<TokenTree>>(vec![
                                            proc_macro::Punct::new(',', proc_macro::Spacing::Alone).into()
                                        ]).into_iter(),
                                    ]).flatten().flatten().collect()
                                ),
                            ).into(),
                            proc_macro::Punct::new(',', proc_macro::Spacing::Alone).into(),
                            proc_macro::Ident::new("vec", proc_macro::Span::call_site()).into(),
                            proc_macro::Punct::new('!', proc_macro::Spacing::Alone).into(),
                            proc_macro::Group::new(
                                proc_macro::Delimiter::Bracket,
                                TokenStream::from_iter::<Vec<TokenTree>>(
                                    self.children.into_iter().map(|c| vec![
                                        c.into_tokens().into_iter(),
                                        TokenStream::from_iter::<Vec<TokenTree>>(vec![
                                            proc_macro::Punct::new(',', proc_macro::Spacing::Alone).into()
                                        ]).into_iter(),
                                    ]).flatten().flatten().collect()
                                ),
                            ).into()
                        ]),
                    ).into()
                ]),
            ).into()
        ])
    }
}

impl StringElement {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        let mut tokens = vec![];

        loop {
            if let Some(t) = input.peek() {
                if let proc_macro::TokenTree::Punct(p) = t {
                    let c = p.as_char();
                    if c == '<' {
                        break;
                    }
                }
                if let proc_macro::TokenTree::Group(g) = t {
                    if g.delimiter() == proc_macro::Delimiter::Brace {
                        break;
                    }
                }
                tokens.push(input.next().unwrap().to_string());
            } else {
                break;
            }
        }

        Ok(Self(tokens.join(" ")))
    }

    fn into_tokens(self) -> TokenStream {
        TokenStream::from_iter::<Vec<TokenTree>>(vec![
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("wasact", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("Element", proc_macro::Span::call_site()).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Joint).into(),
            proc_macro::Punct::new(':', proc_macro::Spacing::Alone).into(),
            proc_macro::Ident::new("Text", proc_macro::Span::call_site()).into(),
            proc_macro::Group::new(
                proc_macro::Delimiter::Parenthesis,
                TokenStream::from_iter::<Vec<TokenTree>>(vec![
                    proc_macro::Literal::string(&self.0).into(),
                    proc_macro::Punct::new('.', proc_macro::Spacing::Alone).into(),
                    proc_macro::Ident::new("to_string", proc_macro::Span::call_site()).into(),
                    proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::new()).into()
                ]),
            ).into()
        ])
    }
}

impl Element {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        let next = input.peek();
        match next {
            Some(t) => {
                if let proc_macro::TokenTree::Punct(p) = t {
                    if p.as_char() == '<' {
                        return Ok(Element::TagElement(TagElement::parse(input)?));
                    }
                }
                if let proc_macro::TokenTree::Group(g) = t {
                    if g.delimiter() == proc_macro::Delimiter::Brace {
                        match input.next().unwrap() {
                            proc_macro::TokenTree::Group(g) =>
                                return Ok(Element::RustElement(g.stream())),
                            _ => unreachable!()
                        }
                    }
                }
                Ok(Element::StringElement(StringElement::parse(input)?))
            }
            None => failure::bail!("Expected token")
        }
    }

    fn into_tokens(self) -> TokenStream {
        match self {
            Element::TagElement(e) => e.into_tokens(),
            Element::StringElement(e) => e.into_tokens(),
            Element::RustElement(e) => TokenStream::from_iter::<Vec<TokenTree>>(vec![
                proc_macro::Group::new(
                    proc_macro::Delimiter::Parenthesis,
                    e,
                ).into(),
                proc_macro::Punct::new('.', proc_macro::Spacing::Alone).into(),
                proc_macro::Ident::new("into", proc_macro::Span::call_site()).into(),
                proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::new()).into()
            ])
        }
    }
}

#[proc_macro_hack]
pub fn html(_item: TokenStream) -> TokenStream {
    let input = match Element::parse(&mut _item.into_iter().peekable()) {
        Ok(i) => i,
        Err(e) => panic!("Error parsing RSX: {}", e)
    };
    input.into_tokens()
}