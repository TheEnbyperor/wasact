extern crate proc_macro;

use proc_macro::{token_stream, TokenStream, TokenTree};
use std::iter::FromIterator;

use proc_macro_hack::proc_macro_hack;

#[derive(Debug)]
struct TagElement {
    tag: String,
    children: Vec<Element>,
}

#[derive(Debug)]
struct StringElement(String);

#[derive(Debug)]
enum Element {
    TagElement(TagElement),
    StringElement(StringElement),
}

fn expect_punct(input: &mut std::iter::Peekable<token_stream::IntoIter>, val: char) -> failure::Fallible<proc_macro::Punct> {
    if let Some(t) = input.next() {
        if let proc_macro::TokenTree::Punct(t) = t {
            if t.as_char() == val {
                Ok(t)
            } else {
                Err(failure::format_err!("Expected {} token, found {}", val, t.as_char()))
            }
        } else {
            Err(failure::format_err!("Expected {} token", val))
        }
    } else {
        Err(failure::format_err!("Expected {} token", val))
    }
}

fn expect_ident(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<proc_macro::Ident> {
    if let Some(t) = input.next() {
        if let proc_macro::TokenTree::Ident(t) = t {
            Ok(t)
        } else {
            Err(failure::format_err!("Expected identifier"))
        }
    } else {
        Err(failure::format_err!("Expected identifier"))
    }
}

impl TagElement {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        expect_punct(input, '<')?;
        let tag = expect_ident(input)?;
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
            children,
        })
    }

    fn into_tokens(self) -> TokenStream {
        TokenStream::from_iter(vec![
            TokenTree::Ident(proc_macro::Ident::new(&"wasact".to_string(), proc_macro::Span::call_site())),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
            TokenTree::Ident(proc_macro::Ident::new(&"Element".to_string(), proc_macro::Span::call_site())),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
            TokenTree::Ident(proc_macro::Ident::new(&"Tag".to_string(), proc_macro::Span::call_site())),
            TokenTree::Group(proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::from_iter(vec![
                TokenTree::Ident(proc_macro::Ident::new(&"wasact".to_string(), proc_macro::Span::call_site())),
                TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
                TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
                TokenTree::Ident(proc_macro::Ident::new(&"TagElement".to_string(), proc_macro::Span::call_site())),
                TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
                TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
                TokenTree::Ident(proc_macro::Ident::new(&"new".to_string(), proc_macro::Span::call_site())),
                TokenTree::Group(proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::from_iter(vec![
                    TokenTree::Literal(proc_macro::Literal::string(&self.tag)),
                    TokenTree::Punct(proc_macro::Punct::new(',', proc_macro::Spacing::Alone)),
                    TokenTree::Ident(proc_macro::Ident::new(&"None".to_string(), proc_macro::Span::call_site())),
                    TokenTree::Punct(proc_macro::Punct::new(',', proc_macro::Spacing::Alone)),
                    TokenTree::Ident(proc_macro::Ident::new(&"vec".to_string(), proc_macro::Span::call_site())),
                    TokenTree::Punct(proc_macro::Punct::new('!', proc_macro::Spacing::Alone)),
                    TokenTree::Group(proc_macro::Group::new(proc_macro::Delimiter::Bracket, TokenStream::from_iter(
                        self.children.into_iter().map(|c| vec![
                            c.into_tokens().into_iter(),
                            TokenStream::from_iter(vec![
                                TokenTree::Punct(proc_macro::Punct::new(',', proc_macro::Spacing::Alone))
                            ]).into_iter(),
                        ]).flatten().flatten()
                    )))
                ].into_iter())))
            ])))
        ])
    }
}

impl StringElement {
    fn parse(input: &mut std::iter::Peekable<token_stream::IntoIter>) -> failure::Fallible<Self> {
        let mut tokens: Vec<_> = vec![];

        loop {
            if let Some(t) = input.peek() {
                if let proc_macro::TokenTree::Punct(p) = t {
                    if p.as_char() == '<' {
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
        TokenStream::from_iter(vec![
            TokenTree::Ident(proc_macro::Ident::new(&"wasact".to_string(), proc_macro::Span::call_site())),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
            TokenTree::Ident(proc_macro::Ident::new(&"Element".to_string(), proc_macro::Span::call_site())),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Joint)),
            TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
            TokenTree::Ident(proc_macro::Ident::new(&"Text".to_string(), proc_macro::Span::call_site())),
            TokenTree::Group(proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::from_iter(vec![
                TokenTree::Literal(proc_macro::Literal::string(&self.0)),
                TokenTree::Punct(proc_macro::Punct::new('.', proc_macro::Spacing::Alone)),
                TokenTree::Ident(proc_macro::Ident::new(&"to_string".to_string(), proc_macro::Span::call_site())),
                TokenTree::Group(proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, TokenStream::new()))
            ].into_iter())))
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
                Ok(Element::StringElement(StringElement::parse(input)?))
            }
            None => failure::bail!("Expected token")
        }
    }

    fn into_tokens(self) -> TokenStream {
        match self {
            Element::TagElement(e) => e.into_tokens(),
            Element::StringElement(e) => e.into_tokens()
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