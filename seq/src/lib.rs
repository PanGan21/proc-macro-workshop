use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[derive(Debug)]
struct SeqMacroInput {
    from: syn::LitInt,
    to: syn::LitInt,
    ident: syn::Ident,
    tt: proc_macro2::TokenStream,
}

impl From<SeqMacroInput> for proc_macro2::TokenStream {
    fn from(val: SeqMacroInput) -> Self {
        let from = val.from.base10_parse::<u64>().unwrap();
        let to = val.to.base10_parse::<u64>().unwrap();

        (from..to).map(|i| val.expand(val.tt.clone(), i)).collect()
    }
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _in = input.parse::<Token![in]>()?;
        let from = syn::LitInt::parse(input)?;
        let _dots = input.parse::<Token![..]>()?;
        let to = syn::LitInt::parse(input)?;
        let content;
        let _braces = syn::braced!(content in input);
        let tt = proc_macro2::TokenStream::parse(&content)?;

        Ok(SeqMacroInput {
            from,
            to,
            tt,
            ident,
        })
    }
}

impl SeqMacroInput {
    fn expand2(
        &self,
        tt: proc_macro2::TokenTree,
        ts: &mut proc_macro2::token_stream::IntoIter,
        i: u64,
    ) -> proc_macro2::TokenTree {
        match tt {
            proc_macro2::TokenTree::Group(g) => {
                let mut expanded =
                    proc_macro2::Group::new(g.delimiter(), self.expand(g.stream(), i));
                expanded.set_span(g.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ref ident) if ident == &self.ident => {
                let mut lit = proc_macro2::Literal::u64_unsuffixed(i);
                lit.set_span(ident.span());
                proc_macro2::TokenTree::Literal(lit)
            }
            proc_macro2::TokenTree::Ident(mut ident) => {
                let mut peek = ts.clone();
                match peek.next() {
                    Some(proc_macro2::TokenTree::Punct(ref punct)) if punct.as_char() == '~' => {
                        match peek.next() {
                            Some(proc_macro2::TokenTree::Ident(ref ident2))
                                if ident2 == &self.ident =>
                            {
                                ident = proc_macro2::Ident::new(
                                    &format!("{}{}", ident, i),
                                    ident.span(),
                                );
                                *ts = peek.clone();

                                match peek.next() {
                                    Some(proc_macro2::TokenTree::Punct(ref punct))
                                        if punct.as_char() == '~' =>
                                    {
                                        *ts = peek;
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                proc_macro2::TokenTree::Ident(ident)
            }
            tt => tt,
        }
    }

    fn expand(&self, stream: proc_macro2::TokenStream, i: u64) -> proc_macro2::TokenStream {
        let mut out = proc_macro2::TokenStream::new();
        let mut tts = stream.into_iter();
        while let Some(tt) = tts.next() {
            out.extend(std::iter::once(self.expand2(tt, &mut tts, i)));
        }
        out
        // stream.into_iter().map(|tt| self.expand2(tt, i)).collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}
