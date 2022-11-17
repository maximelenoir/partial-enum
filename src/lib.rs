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
/// ```rust
/// #[derive(partial_enum::Enum)]
/// enum Error {
///     Foo(Foo),
///     Bar(Bar),
/// }
/// ```
///
/// will generate the following enum:
///
/// ```rust
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
