use crate::{from_reflect, impls, ReflectDerive, REFLECT_ATTRIBUTE_NAME};
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Token, TypePath};

/// Generates the remote wrapper type and implements all the necessary traits.
pub(crate) fn reflect_remote(args: TokenStream, input: TokenStream) -> TokenStream {
    let remote_args = match syn::parse::<RemoteArgs>(args) {
        Ok(path) => path,
        Err(err) => return err.to_compile_error().into(),
    };

    let is_from_reflect = remote_args.is_from_reflect;
    let remote_ty = remote_args.remote_ty;

    let ast = parse_macro_input!(input as DeriveInput);
    let wrapper_definition = generate_remote_wrapper(&ast, &remote_ty);

    let mut derive_data = match ReflectDerive::from_input(&ast) {
        Ok(data) => data,
        Err(err) => return err.into_compile_error().into(),
    };

    derive_data.set_remote(Some(&remote_ty));

    let from_reflect_impl = if is_from_reflect {
        Some(from_reflect_remote(&derive_data))
    } else {
        None
    };

    let trait_impls = match derive_data {
        ReflectDerive::Struct(struct_data) | ReflectDerive::UnitStruct(struct_data) => {
            proc_macro2::TokenStream::from(impls::impl_struct(&struct_data))
        }
        ReflectDerive::TupleStruct(struct_data) => {
            proc_macro2::TokenStream::from(impls::impl_tuple_struct(&struct_data))
        }
        ReflectDerive::Enum(meta) => proc_macro2::TokenStream::from(impls::impl_enum(&meta)),
        _ => {
            return syn::Error::new(ast.span(), "cannot reflect a remote value type")
                .into_compile_error()
                .into()
        }
    };

    TokenStream::from(quote! {
        #wrapper_definition

        #from_reflect_impl

        #trait_impls
    })
}

/// Return the generated `FromReflect` impl for the given derive data.
fn from_reflect_remote(derive_data: &ReflectDerive) -> proc_macro2::TokenStream {
    proc_macro2::TokenStream::from(match derive_data {
        ReflectDerive::Struct(struct_data) | ReflectDerive::UnitStruct(struct_data) => {
            from_reflect::impl_struct(struct_data)
        }
        ReflectDerive::TupleStruct(struct_data) => from_reflect::impl_tuple_struct(struct_data),
        ReflectDerive::Enum(meta) => from_reflect::impl_enum(meta),
        ReflectDerive::Value(meta) => from_reflect::impl_value(meta),
    })
}

/// Generates the remote wrapper type.
///
/// # Example
///
/// If the supplied remote type is `Bar<T>`, then the wrapper type— named `Foo<T>`— would look like:
///
/// ```
/// # struct Bar<T>(T);
///
/// #[repr(transparent)]
/// struct Foo<T>(Bar<T>);
/// ```
fn generate_remote_wrapper(input: &DeriveInput, remote_ty: &TypePath) -> proc_macro2::TokenStream {
    let ident = &input.ident;
    let vis = &input.vis;
    let ty_generics = &input.generics;
    let where_clause = &input.generics.where_clause;
    let attrs = input
        .attrs
        .iter()
        .filter(|attr| !attr.path.is_ident(REFLECT_ATTRIBUTE_NAME));

    quote! {
        #(#attrs)*
        #[repr(transparent)]
        #vis struct #ident #ty_generics (pub #remote_ty) #where_clause;
    }
}

/// Metadata from the arguments defined in the `reflect_remote` attribute.
///
/// The syntax for the arguments is: `#[reflect_remote(REMOTE_TYPE_PATH)]`
/// or `#[reflect_remote(REMOTE_TYPE_PATH, FromReflect)]`.
/// Please note that we expect the `FromReflect` _ident_— we do not verify/handle the path to `FromReflect`.
struct RemoteArgs {
    remote_ty: TypePath,
    is_from_reflect: bool,
}

impl Parse for RemoteArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let remote_ty = input.parse()?;
        input.parse::<Option<Token![,]>>()?;
        let is_from_reflect = input.parse::<Option<keywords::FromReflect>>()?.is_some();

        Ok(Self {
            remote_ty,
            is_from_reflect,
        })
    }
}

mod keywords {
    syn::custom_keyword!(FromReflect);
}
