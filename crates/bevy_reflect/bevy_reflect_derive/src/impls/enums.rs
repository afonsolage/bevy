use crate::derive_data::{EnumVariantFields, ReflectEnum, StructField};
use crate::enum_utility::{get_variant_constructors, EnumVariantConstructors};
use crate::impls::any::impl_reflect_any_methods;
use crate::impls::{impl_type_path, impl_typed};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;

pub(crate) fn impl_enum(reflect_enum: &ReflectEnum) -> TokenStream {
    let bevy_reflect_path = reflect_enum.meta().bevy_reflect_path();
    let enum_name = reflect_enum.meta().type_name();
    let is_remote = reflect_enum.is_remote_wrapper();

    // For `match self` expressions where self is a reference
    let match_this = if is_remote {
        quote!(&self.0)
    } else {
        quote!(self)
    };
    // For `match self` expressions where self is a mutable reference
    let match_this_mut = if is_remote {
        quote!(&mut self.0)
    } else {
        quote!(self)
    };
    // For `*self` assignments
    let deref_this = if is_remote {
        quote!(self.0)
    } else {
        quote!(*self)
    };

    let ref_name = Ident::new("__name_param", Span::call_site());
    let ref_index = Ident::new("__index_param", Span::call_site());
    let ref_value = Ident::new("__value_param", Span::call_site());

    let EnumImpls {
        variant_info,
        enum_field,
        enum_field_mut,
        enum_field_at,
        enum_field_at_mut,
        enum_index_of,
        enum_name_at,
        enum_field_len,
        enum_variant_name,
        enum_variant_index,
        enum_variant_type,
    } = generate_impls(reflect_enum, &ref_index, &ref_name);

    let EnumVariantConstructors {
        variant_names,
        variant_constructors,
    } = get_variant_constructors(reflect_enum, &ref_value, true);

    let hash_fn = reflect_enum
        .meta()
        .traits()
        .get_hash_impl(bevy_reflect_path)
        .unwrap_or_else(|| {
            quote! {
                fn reflect_hash(&self) -> Option<u64> {
                    #bevy_reflect_path::enum_hash(self)
                }
            }
        });
    let debug_fn = reflect_enum.meta().traits().get_debug_impl();
    let partial_eq_fn = reflect_enum
        .meta()
        .traits()
        .get_partial_eq_impl(bevy_reflect_path)
        .unwrap_or_else(|| {
            quote! {
                fn reflect_partial_eq(&self, value: &dyn #bevy_reflect_path::Reflect) -> Option<bool> {
                    #bevy_reflect_path::enum_partial_eq(self, value)
                }
            }
        });

    let string_name = enum_name.to_string();
    let typed_impl = impl_typed(
        enum_name,
        reflect_enum.meta().generics(),
        quote! {
            let variants = [#(#variant_info),*];
            let info = #bevy_reflect_path::EnumInfo::new::<Self>(#string_name, &variants);
            #bevy_reflect_path::TypeInfo::Enum(info)
        },
        bevy_reflect_path,
    );

    let any_impls = impl_reflect_any_methods(reflect_enum.is_remote_wrapper());

    let type_path_impl = impl_type_path(reflect_enum.meta());

    let get_type_registration_impl = reflect_enum.meta().get_type_registration();
    let (impl_generics, ty_generics, where_clause) =
        reflect_enum.meta().generics().split_for_impl();

    TokenStream::from(quote! {
        #get_type_registration_impl

        #typed_impl

        #type_path_impl

        impl #impl_generics #bevy_reflect_path::Enum for #enum_name #ty_generics #where_clause {
            fn field(&self, #ref_name: &str) -> Option<&dyn #bevy_reflect_path::Reflect> {
                 match #match_this {
                    #(#enum_field,)*
                    _ => None,
                }
            }

            fn field_at(&self, #ref_index: usize) -> Option<&dyn #bevy_reflect_path::Reflect> {
                match #match_this {
                    #(#enum_field_at,)*
                    _ => None,
                }
            }

            fn field_mut(&mut self, #ref_name: &str) -> Option<&mut dyn #bevy_reflect_path::Reflect> {
                 match #match_this_mut {
                    #(#enum_field_mut,)*
                    _ => None,
                }
            }

            fn field_at_mut(&mut self, #ref_index: usize) -> Option<&mut dyn #bevy_reflect_path::Reflect> {
                match #match_this_mut {
                    #(#enum_field_at_mut,)*
                    _ => None,
                }
            }

            fn index_of(&self, #ref_name: &str) -> Option<usize> {
                 match #match_this {
                    #(#enum_index_of,)*
                    _ => None,
                }
            }

            fn name_at(&self, #ref_index: usize) -> Option<&str> {
                 match #match_this {
                    #(#enum_name_at,)*
                    _ => None,
                }
            }

            fn iter_fields(&self) -> #bevy_reflect_path::VariantFieldIter {
                #bevy_reflect_path::VariantFieldIter::new(self)
            }

            #[inline]
            fn field_len(&self) -> usize {
                 match #match_this {
                    #(#enum_field_len,)*
                    _ => 0,
                }
            }

            #[inline]
            fn variant_name(&self) -> &str {
                 match #match_this {
                    #(#enum_variant_name,)*
                    _ => unreachable!(),
                }
            }

            #[inline]
            fn variant_index(&self) -> usize {
                 match #match_this {
                    #(#enum_variant_index,)*
                    _ => unreachable!(),
                }
            }

            #[inline]
            fn variant_type(&self) -> #bevy_reflect_path::VariantType {
                 match #match_this {
                    #(#enum_variant_type,)*
                    _ => unreachable!(),
                }
            }

            fn clone_dynamic(&self) -> #bevy_reflect_path::DynamicEnum {
                #bevy_reflect_path::DynamicEnum::from_ref::<Self>(self)
            }
        }

        impl #impl_generics #bevy_reflect_path::Reflect for #enum_name #ty_generics #where_clause {
            #[inline]
            fn type_path(&self) -> &str {
                <Self as #bevy_reflect_path::TypePath>::type_path()
            }

            #[inline]
            fn get_type_info(&self) -> &'static #bevy_reflect_path::TypeInfo {
                <Self as #bevy_reflect_path::Typed>::type_info()
            }

            #any_impls

            #[inline]
            fn as_reflect(&self) -> &dyn #bevy_reflect_path::Reflect {
                self
            }

            #[inline]
            fn as_reflect_mut(&mut self) -> &mut dyn #bevy_reflect_path::Reflect {
                self
            }

            #[inline]
            fn clone_value(&self) -> Box<dyn #bevy_reflect_path::Reflect> {
                Box::new(#bevy_reflect_path::Enum::clone_dynamic(self))
            }

            #[inline]
            fn set(&mut self, #ref_value: Box<dyn #bevy_reflect_path::Reflect>) -> Result<(), Box<dyn #bevy_reflect_path::Reflect>> {
                *self = #ref_value.take()?;
                Ok(())
            }

            #[inline]
            fn apply(&mut self, #ref_value: &dyn #bevy_reflect_path::Reflect) {
                if let #bevy_reflect_path::ReflectRef::Enum(#ref_value) = #ref_value.reflect_ref() {
                    if #bevy_reflect_path::Enum::variant_name(self) == #ref_value.variant_name() {
                        // Same variant -> just update fields
                        match #ref_value.variant_type() {
                            #bevy_reflect_path::VariantType::Struct => {
                                for field in #ref_value.iter_fields() {
                                    let name = field.name().unwrap();
                                    #bevy_reflect_path::Enum::field_mut(self, name).map(|v| v.apply(field.value()));
                                }
                            }
                            #bevy_reflect_path::VariantType::Tuple => {
                                for (index, field) in #ref_value.iter_fields().enumerate() {
                                    #bevy_reflect_path::Enum::field_at_mut(self, index).map(|v| v.apply(field.value()));
                                }
                            }
                            _ => {}
                        }
                    } else {
                        // New variant -> perform a switch
                        match #ref_value.variant_name() {
                            #(#variant_names => {
                                #deref_this = #variant_constructors
                            })*
                            name => panic!("variant with name `{}` does not exist on enum `{}`", name, self.type_path()),
                        }
                    }
                } else {
                    panic!("`{}` is not an enum", #ref_value.type_path());
                }
            }

            fn reflect_ref(&self) -> #bevy_reflect_path::ReflectRef {
                #bevy_reflect_path::ReflectRef::Enum(self)
            }

            fn reflect_mut(&mut self) -> #bevy_reflect_path::ReflectMut {
                #bevy_reflect_path::ReflectMut::Enum(self)
            }

            #hash_fn

            #partial_eq_fn

            #debug_fn
        }
    })
}

struct EnumImpls {
    variant_info: Vec<proc_macro2::TokenStream>,
    enum_field: Vec<proc_macro2::TokenStream>,
    enum_field_mut: Vec<proc_macro2::TokenStream>,
    enum_field_at: Vec<proc_macro2::TokenStream>,
    enum_field_at_mut: Vec<proc_macro2::TokenStream>,
    enum_index_of: Vec<proc_macro2::TokenStream>,
    enum_name_at: Vec<proc_macro2::TokenStream>,
    enum_field_len: Vec<proc_macro2::TokenStream>,
    enum_variant_name: Vec<proc_macro2::TokenStream>,
    enum_variant_index: Vec<proc_macro2::TokenStream>,
    enum_variant_type: Vec<proc_macro2::TokenStream>,
}

fn generate_impls(reflect_enum: &ReflectEnum, ref_index: &Ident, ref_name: &Ident) -> EnumImpls {
    let bevy_reflect_path = reflect_enum.meta().bevy_reflect_path();

    let mut variant_info = Vec::new();
    let mut enum_field = Vec::new();
    let mut enum_field_mut = Vec::new();
    let mut enum_field_at = Vec::new();
    let mut enum_field_at_mut = Vec::new();
    let mut enum_index_of = Vec::new();
    let mut enum_name_at = Vec::new();
    let mut enum_field_len = Vec::new();
    let mut enum_variant_name = Vec::new();
    let mut enum_variant_index = Vec::new();
    let mut enum_variant_type = Vec::new();

    for (variant_index, variant) in reflect_enum.variants().iter().enumerate() {
        let ident = &variant.data.ident;
        let name = ident.to_string();
        let unit = reflect_enum.get_unit(ident);

        enum_variant_name.push(quote! {
            #unit{..} => #name
        });
        enum_variant_index.push(quote! {
            #unit{..} => #variant_index
        });

        fn for_fields(
            fields: &[StructField],
            mut generate_for_field: impl FnMut(usize, usize, &StructField) -> proc_macro2::TokenStream,
        ) -> (usize, Vec<proc_macro2::TokenStream>) {
            let mut constructor_argument = Vec::new();
            let mut reflect_idx = 0;
            for field in fields.iter() {
                if field.attrs.ignore.is_ignored() {
                    // Ignored field
                    continue;
                }
                constructor_argument.push(generate_for_field(reflect_idx, field.index, field));
                reflect_idx += 1;
            }
            (reflect_idx, constructor_argument)
        }

        /// Process the field value to account for remote types.
        ///
        /// If the field is a remote type, then the value will be transmuted accordingly.
        fn process_field_value(
            ident: &Ident,
            field: &StructField,
            is_mutable: bool,
        ) -> proc_macro2::TokenStream {
            let ref_token = if is_mutable { quote!(&mut) } else { quote!(&) };
            field
                .attrs
                .remote
                .as_ref()
                .map(|ty| {
                    quote! {
                        // SAFE: The wrapper type should be repr(transparent) over the remote type
                        unsafe { ::std::mem::transmute::<#ref_token _, #ref_token #ty>(#ident) }
                    }
                })
                .unwrap_or_else(|| quote!(#ident))
        }

        let mut add_fields_branch = |variant, info_type, arguments, field_len| {
            let variant = Ident::new(variant, Span::call_site());
            let info_type = Ident::new(info_type, Span::call_site());
            variant_info.push(quote! {
                #bevy_reflect_path::VariantInfo::#variant(
                    #bevy_reflect_path::#info_type::new(#arguments)
                )
            });
            enum_field_len.push(quote! {
                #unit{..} => #field_len
            });
            enum_variant_type.push(quote! {
                #unit{..} => #bevy_reflect_path::VariantType::#variant
            });
        };
        match &variant.fields {
            EnumVariantFields::Unit => {
                add_fields_branch("Unit", "UnitVariantInfo", quote!(#name), 0usize);
            }
            EnumVariantFields::Unnamed(fields) => {
                let (field_len, argument) = for_fields(fields, |reflect_idx, declar, field| {
                    let declar_field = syn::Index::from(declar);

                    let __value = Ident::new("__value", Span::call_site());
                    let value_ref = process_field_value(&__value, field, false);
                    let value_mut = process_field_value(&__value, field, true);
                    enum_field_at.push(quote! {
                        #unit { #declar_field : #__value, .. } if #ref_index == #reflect_idx => Some(#value_ref)
                    });
                    enum_field_at_mut.push(quote! {
                        #unit { #declar_field : #__value, .. } if #ref_index == #reflect_idx => Some(#value_mut)
                    });
                    let field_ty = &field.reflected_type();
                    quote! { #bevy_reflect_path::UnnamedField::new::<#field_ty>(#reflect_idx) }
                });
                let arguments = quote!(#name, &[ #(#argument),* ]);
                add_fields_branch("Tuple", "TupleVariantInfo", arguments, field_len);
            }
            EnumVariantFields::Named(fields) => {
                let (field_len, argument) = for_fields(fields, |reflect_idx, _, field| {
                    let field_ident = field.data.ident.as_ref().unwrap();
                    let field_name = field_ident.to_string();

                    let __value = Ident::new("__value", Span::call_site());
                    let value_ref = process_field_value(&__value, field, false);
                    let value_mut = process_field_value(&__value, field, true);
                    enum_field.push(quote! {
                        #unit{ #field_ident: #__value, .. } if #ref_name == #field_name => Some(#value_ref)
                    });
                    enum_field_mut.push(quote! {
                        #unit{ #field_ident: #__value, .. } if #ref_name == #field_name => Some(#value_mut)
                    });
                    enum_field_at.push(quote! {
                        #unit{ #field_ident: #__value, .. } if #ref_index == #reflect_idx => Some(#value_ref)
                    });
                    enum_field_at_mut.push(quote! {
                        #unit{ #field_ident: #__value, .. } if #ref_index == #reflect_idx => Some(#value_mut)
                    });
                    enum_index_of.push(quote! {
                        #unit{ .. } if #ref_name == #field_name => Some(#reflect_idx)
                    });
                    enum_name_at.push(quote! {
                        #unit{ .. } if #ref_index == #reflect_idx => Some(#field_name)
                    });

                    let field_ty = &field.reflected_type();
                    quote! { #bevy_reflect_path::NamedField::new::<#field_ty>(#field_name) }
                });
                let arguments = quote!(#name, &[ #(#argument),* ]);
                add_fields_branch("Struct", "StructVariantInfo", arguments, field_len);
            }
        };
    }

    EnumImpls {
        variant_info,
        enum_field,
        enum_field_mut,
        enum_field_at,
        enum_field_at_mut,
        enum_index_of,
        enum_name_at,
        enum_field_len,
        enum_variant_name,
        enum_variant_index,
        enum_variant_type,
    }
}
