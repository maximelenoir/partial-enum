#![feature(never_type)]
#![feature(exhaustive_patterns)]

//! A proc-macro for generating partial enums from a template enum. This partial
//! enum contains the same number of variants as the template but can disable a
//! subset of these variants at compile time. The goal is used specialize enum
//! with finer-grained variant set for each API.
//!
//! This is useful for handling errors. A common pattern is to define an enum
//! with all possible errors and use this for the entire API surface. Albeit
//! simple, this representation can fail to represent exact error scenarii by
//! allowing errors that can not happen.
//!
//! Take an API responsible for decoding messages from a socket.
//!
//! ```
//! # struct ConnectError;
//! # struct ReadError;
//! # struct DecodeError;
//! # struct Socket;
//! # struct Bytes;
//! # struct Message;
//! enum Error {
//!     Connect(ConnectError),
//!     Read(ReadError),
//!     Decode(DecodeError),
//! }
//!
//! fn connect() -> Result<Socket, Error> { Ok(Socket) }
//! fn read(sock: &mut Socket) -> Result<Bytes, Error> { Ok(Bytes) }
//! fn decode(bytes: Bytes) -> Result<Message, Error> { Err(Error::Decode(DecodeError)) }
//! ```
//!
//! The same error enum is used all over the place and exposes variants that do
//! not match the API: `decode` returns a `DecodeError` but nothing prevents
//! from returning a `ConnectError`. For such low-level API, we could substitute
//! `Error` by their matching error like `ConnectError` for `connect`. The
//! downside is that composing with such functions forces us to redefine custom
//! enums:
//!
//! ```
//! # struct ReadError;
//! # struct DecodeError;
//! # struct Socket;
//! # struct Bytes;
//! # struct Message;
//! enum NextMessageError {
//!     Read(ReadError),
//!     Decode(DecodeError),
//! }
//!
//! impl From<ReadError> for NextMessageError {
//!     fn from(err: ReadError) -> Self {
//!         NextMessageError::Read(err)
//!     }
//! }
//!
//! impl From<DecodeError> for NextMessageError {
//!     fn from(err: DecodeError) -> Self {
//!         NextMessageError::Decode(err)
//!     }
//! }
//!
//! fn read(sock: &mut Socket) -> Result<Bytes, ReadError> { Ok(Bytes) }
//! fn decode(bytes: Bytes) -> Result<Message, DecodeError> { Err(DecodeError) }
//! fn next_message(sock: &mut Socket) -> Result<Message, NextMessageError> {
//!     let payload = read(sock)?;
//!     let message = decode(payload)?;
//!     Ok(message)
//! }
//! ```
//!
//! This proc-macro intend to ease the composition of APIs that does not share
//! the exact same errors by generating a new generic enum where each variant
//! can be disabled one by one. We can then redefine our API like so:
//!
//! ```
//! # #![feature(never_type)]
//! # mod example {
//! # struct ConnectError;
//! # struct ReadError;
//! # struct DecodeError;
//! # struct Socket;
//! # struct Bytes;
//! # struct Message;
//! #[derive(partial_enum::Enum)]
//! enum Error {
//!     Connect(ConnectError),
//!     Read(ReadError),
//!     Decode(DecodeError),
//! }
//!
//! use partial::Error as E;
//!
//! fn connect() -> Result<Socket, E<ConnectError, !, !>> { Ok(Socket) }
//! fn read(sock: &mut Socket) -> Result<Bytes, E<!, ReadError, !>> { Ok(Bytes) }
//! fn decode(bytes: Bytes) -> Result<Message, E<!, !, DecodeError>> { Err(E::Decode(DecodeError)) }
//! fn next_message(sock: &mut Socket) -> Result<Message, E<!, ReadError, DecodeError>> {
//!     let payload = read(sock)?;
//!     let message = decode(payload)?;
//!     Ok(message)
//! }
//! # }
//! ```
//!
//! Notice that the `next_message` implementation is unaltered and the signature
//! clearly states that only `ReadError` and `DecodeError` can be returned. The
//! callee would never be able to match on `Error::Connect`. By using the
//! nightly feature `exhaustive_patterns`, the match statement does not even
//! need to write the disabled variants.
//!
//! ```
//! #![feature(exhaustive_patterns)]
//! # #![feature(never_type)]
//! # mod example {
//! # struct ConnectError;
//! # struct ReadError;
//! # struct DecodeError;
//! # struct Socket;
//! # struct Bytes;
//! # struct Message;
//! # #[derive(partial_enum::Enum)]
//! # enum Error {
//! #     Connect(ConnectError),
//! #     Read(ReadError),
//! #     Decode(DecodeError),
//! # }
//! # use partial::Error as E;
//! # fn connect() -> Result<Socket, E<ConnectError, !, !>> { Ok(Socket) }
//! # fn read(sock: &mut Socket) -> Result<Bytes, E<!, ReadError, !>> { Ok(Bytes) }
//! # fn decode(bytes: Bytes) -> Result<Message, E<!, !, DecodeError>> { Err(E::Decode(DecodeError)) }
//! # fn next_message(sock: &mut Socket) -> Result<Message, E<!, ReadError, DecodeError>> {
//! #     let payload = read(sock)?;
//! #     let message = decode(payload)?;
//! #     Ok(message)
//! # }
//! fn read_one_message() -> Result<Message, Error> {
//!     let mut socket = connect()?;
//!     match next_message(&mut socket) {
//!         Ok(msg) => Ok(msg),
//!         Err(E::Read(_)) => {
//!             // Retry...
//!             next_message(&mut socket).map_err(Error::from)
//!         }
//!         Err(E::Decode(err)) => Err(Error::Decode(err)),
//!     }
//! }
//! # }
//! ```

extern crate proc_macro;
use permutation::Permutations;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Fields, Ident, ItemEnum, Token, Type, TypeNever, Visibility,
};

mod permutation;

/// Create the partial version of this enum.
///
/// This macro generates another enum of the same name, in a sub-module called
/// `partial`. This enum have the same variant identifiers as the original but
/// each associated type is now generic: an enum with `N` variants will have `N`
/// generic parameters. Each of those types can be instantiated with either the
/// original type or the never type `!`. No other type can be substituted. This
/// effectively creates an enum capable of disabling several variants. The enum
/// with no disabled variant is functionally equivalent to the original enum.
///
/// # Restrictions
///
/// Some restrictions are applied on the original enum for the macro to work:
///
/// * generic parameters are not supported
/// * named variant are not supported
/// * unit variant are not supported
/// * unnamed variants must only contain one type
///
/// # Example
///
/// The following `derive` statement:
///
/// ```
/// # #![feature(never_type)]
/// # mod example {
/// # struct Foo;
/// # struct Bar;
/// #[derive(partial_enum::Enum)]
/// enum Error {
///     Foo(Foo),
///     Bar(Bar),
/// }
/// # }
/// ```
///
/// will generate the following enum:
///
/// ```
/// mod partial {
///     enum Error<Foo, Bar> {
///         Foo(Foo),
///         Bar(Bar),
///     }
/// }
/// ```
///
/// where `Foo` can only be instantiated by `Foo` or `!` and `Bar` can only be
/// instantiated by `Bar` or `!`. `From` implementations are provided for all
/// valid morphisms: such conversion is valid if and only if, for each variant
/// type, we never go from a non-`!` type to the `!` type. This would otherwise
/// allow to forget this variant and pretend we can never match on it. The
/// compiler will rightfully complains that we're trying to instantiate an
/// uninhabited type.
#[proc_macro_derive(Enum)]
pub fn derive_error(item: TokenStream) -> TokenStream {
    let e: Enum = syn::parse_macro_input!(item as Enum);
    e.to_tokens().to_token_stream().into()
}

struct Enum(PartialEnum);

#[derive(Clone)]
struct PartialEnum {
    vis: Visibility,
    ident: Ident,
    variants: Vec<Variant>,
}

#[derive(Clone)]
struct Variant {
    ident: Ident,
    typ: Type,
}

impl Parse for Enum {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let enum_: ItemEnum = input.parse()?;
        if !enum_.generics.params.is_empty() {
            return Err(syn::Error::new(
                enum_.span(),
                "generic parameters are not supported",
            ));
        }

        let mut variants = vec![];
        for variant in enum_.variants.into_iter() {
            match variant.fields {
                Fields::Named(_) => {
                    return Err(syn::Error::new(
                        variant.fields.span(),
                        "named field is not supported",
                    ))
                }
                Fields::Unnamed(ref fields) if fields.unnamed.len() != 1 => {
                    return Err(syn::Error::new(
                        variant.fields.span(),
                        "only one field is supported",
                    ))
                }
                Fields::Unnamed(mut fields) => {
                    let field = fields.unnamed.pop().unwrap().into_value();
                    variants.push(Variant {
                        ident: variant.ident,
                        typ: field.ty,
                    });
                }
                Fields::Unit => {
                    return Err(syn::Error::new(
                        variant.fields.span(),
                        "unit field is not supported",
                    ))
                }
            }
        }

        Ok(Enum(PartialEnum {
            vis: enum_.vis,
            ident: enum_.ident,
            variants,
        }))
    }
}

impl Enum {
    fn to_tokens(&self) -> impl ToTokens {
        let enum_vis = &self.vis;
        let enum_name = quote::format_ident!("{}", self.ident);

        let mut variant_generics = vec![];
        let mut variant_traits = vec![];
        let mut variant_idents = vec![];
        let mut variant_types = vec![];
        for variant in &self.variants {
            variant_generics.push(quote::format_ident!("{}", variant.ident));
            variant_traits.push(quote::format_ident!("{}Bound", variant.ident));
            variant_idents.push(&variant.ident);
            variant_types.push(&variant.typ);
        }

        let mut from_impls = vec![];
        for to in self.generate_all_partial_enums() {
            let to_type = to.enum_tokens();
            for from in self.generate_convertible_partial_enums(&to) {
                let from_type = from.enum_tokens();
                from_impls.push(quote::quote!(
                    impl From<#from_type> for #to_type {
                        fn from(value: #from_type) -> Self {
                            #[allow(unreachable_code)]
                            match value {
                                #(#enum_name::#variant_idents(x) => Self::#variant_idents(x),)*
                            }
                        }
                    }
                ));
            }
            from_impls.push(quote::quote!(
                impl From<#to_type> for super::#enum_name {
                    fn from(value: #to_type) -> Self {
                        #[allow(unreachable_code)]
                        match value {
                            #(#enum_name::#variant_idents(x) => Self::#variant_idents(x),)*
                        }
                    }
                }

            ));
        }

        quote::quote!(
            #enum_vis mod partial {
                #(use super::#variant_types;)*

                pub enum #enum_name<#(#variant_generics: #variant_traits),*> {
                    #(#variant_idents(#variant_generics)),*
                }

                #(
                pub trait #variant_traits {}
                impl #variant_traits for #variant_types {}
                impl #variant_traits for ! {}
                )*

                #(#from_impls)*
            }
        )
    }

    fn generate_all_partial_enums(&self) -> Vec<PartialEnum> {
        let span = Span::call_site();
        let never_type = Type::Never(TypeNever {
            bang_token: Token![!]([span]),
        });

        let mut enums = vec![];
        for perm in Permutations::new(self.variants.len()) {
            let mut enum_ = self.0.clone();
            for (i, is_concrete) in perm.enumerate() {
                if !is_concrete {
                    enum_.variants[i].typ = never_type.clone();
                }
            }
            enums.push(enum_);
        }
        enums
    }

    fn generate_convertible_partial_enums(&self, to: &PartialEnum) -> Vec<PartialEnum> {
        self.generate_all_partial_enums()
            .into_iter()
            .filter(|from| from.is_convertible_to(to))
            .filter(|from| from != to)
            .collect()
    }
}

impl std::ops::Deref for Enum {
    type Target = PartialEnum;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for PartialEnum {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.variants == other.variants
    }
}

impl PartialEnum {
    fn enum_tokens(&self) -> impl ToTokens {
        let enum_name = &self.ident;
        let variant_types = self.variants.iter().map(|variant| &variant.typ);
        quote::quote!(#enum_name<#(#variant_types,)*>)
    }

    fn is_convertible_to(&self, to: &PartialEnum) -> bool {
        assert_eq!(self.variants.len(), to.variants.len());
        for (from, to) in self.variants.iter().zip(&to.variants) {
            if from.is_concrete() && to.is_never() {
                return false;
            }
        }
        true
    }
}

impl Variant {
    fn is_never(&self) -> bool {
        matches!(self.typ, Type::Never(_))
    }

    fn is_concrete(&self) -> bool {
        !self.is_never()
    }
}

impl PartialEq for Variant {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.is_concrete() == other.is_concrete()
    }
}
