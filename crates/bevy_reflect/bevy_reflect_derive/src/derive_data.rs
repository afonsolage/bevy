use crate::container_attributes::ReflectTraits;
use crate::field_attributes::{parse_field_attrs, ReflectFieldAttr};
use crate::utility::members_to_serialization_denylist;
use bit_set::BitSet;
use quote::{quote, ToTokens};

use crate::{utility, REFLECT_ATTRIBUTE_NAME, REFLECT_VALUE_ATTRIBUTE_NAME};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    Data, DeriveInput, Field, Fields, Generics, Ident, Meta, Path, Token, Type, TypePath, Variant,
};

pub(crate) enum ReflectDerive<'a> {
    Struct(ReflectStruct<'a>),
    TupleStruct(ReflectStruct<'a>),
    UnitStruct(ReflectStruct<'a>),
    Enum(ReflectEnum<'a>),
    Value(ReflectMeta<'a>),
}

/// Metadata present on all reflected types, including name, generics, and attributes.
///
/// # Example
///
/// ```ignore
/// #[derive(Reflect)]
/// //                          traits
/// //        |----------------------------------------|
/// #[reflect(PartialEq, Serialize, Deserialize, Default)]
/// //            type_name       generics
/// //     |-------------------||----------|
/// struct ThingThatImReflecting<T1, T2, T3> {/* ... */}
/// ```
pub(crate) struct ReflectMeta<'a> {
    /// The registered traits for this type.
    traits: ReflectTraits,
    /// The name of this type.
    type_name: &'a Ident,
    /// The generics defined on this type.
    generics: &'a Generics,
    /// A cached instance of the path to the `bevy_reflect` crate.
    bevy_reflect_path: Path,
}

/// Struct data used by derive macros for `Reflect` and `FromReflect`.
///
/// # Example
///
/// ```ignore
/// #[derive(Reflect)]
/// #[reflect(PartialEq, Serialize, Deserialize, Default)]
/// struct ThingThatImReflecting<T1, T2, T3> {
///     x: T1, // |
///     y: T2, // |- fields
///     z: T3  // |
/// }
/// ```
pub(crate) struct ReflectStruct<'a> {
    meta: ReflectMeta<'a>,
    serialization_denylist: BitSet<u32>,
    fields: Vec<StructField<'a>>,
    remote_ty: Option<&'a TypePath>,
}

/// Enum data used by derive macros for `Reflect` and `FromReflect`.
///
/// # Example
///
/// ```ignore
/// #[derive(Reflect)]
/// #[reflect(PartialEq, Serialize, Deserialize, Default)]
/// enum ThingThatImReflecting<T1, T2, T3> {
///     A(T1),                  // |
///     B,                      // |- variants
///     C { foo: T2, bar: T3 }  // |
/// }
/// ```
pub(crate) struct ReflectEnum<'a> {
    meta: ReflectMeta<'a>,
    variants: Vec<EnumVariant<'a>>,
    remote_ty: Option<&'a TypePath>,
}

/// Represents a field on a struct or tuple struct.
pub(crate) struct StructField<'a> {
    /// The raw field.
    pub data: &'a Field,
    /// The reflection-based attributes on the field.
    pub attrs: ReflectFieldAttr,
    /// The index of this field within the struct.
    pub index: usize,
}

/// Represents a variant on an enum.
pub(crate) struct EnumVariant<'a> {
    /// The raw variant.
    pub data: &'a Variant,
    /// The fields within this variant.
    pub fields: EnumVariantFields<'a>,
    /// The reflection-based attributes on the variant.
    #[allow(dead_code)]
    pub attrs: ReflectFieldAttr,
    /// The index of this variant within the enum.
    #[allow(dead_code)]
    pub index: usize,
}

pub(crate) enum EnumVariantFields<'a> {
    Named(Vec<StructField<'a>>),
    Unnamed(Vec<StructField<'a>>),
    Unit,
}

impl<'a> ReflectDerive<'a> {
    pub fn from_input(input: &'a DeriveInput) -> Result<Self, syn::Error> {
        let mut traits = ReflectTraits::default();
        // Should indicate whether `#[reflect_value]` was used
        let mut force_reflect_value = false;

        for attribute in input.attrs.iter().filter_map(|attr| attr.parse_meta().ok()) {
            match attribute {
                Meta::List(meta_list) if meta_list.path.is_ident(REFLECT_ATTRIBUTE_NAME) => {
                    traits = ReflectTraits::from_nested_metas(&meta_list.nested);
                }
                Meta::List(meta_list) if meta_list.path.is_ident(REFLECT_VALUE_ATTRIBUTE_NAME) => {
                    force_reflect_value = true;
                    traits = ReflectTraits::from_nested_metas(&meta_list.nested);
                }
                Meta::Path(path) if path.is_ident(REFLECT_VALUE_ATTRIBUTE_NAME) => {
                    force_reflect_value = true;
                }
                _ => continue,
            }
        }
        if force_reflect_value {
            return Ok(Self::Value(ReflectMeta::new(
                &input.ident,
                &input.generics,
                traits,
            )));
        }

        return match &input.data {
            Data::Struct(data) => {
                let fields = Self::collect_struct_fields(&data.fields)?;
                let meta = ReflectMeta::new(&input.ident, &input.generics, traits);
                let reflect_struct = ReflectStruct {
                    meta,
                    serialization_denylist: members_to_serialization_denylist(
                        fields.iter().map(|v| v.attrs.ignore),
                    ),
                    fields,
                    remote_ty: None,
                };

                match data.fields {
                    Fields::Named(..) => Ok(Self::Struct(reflect_struct)),
                    Fields::Unnamed(..) => Ok(Self::TupleStruct(reflect_struct)),
                    Fields::Unit => Ok(Self::UnitStruct(reflect_struct)),
                }
            }
            Data::Enum(data) => {
                let variants = Self::collect_enum_variants(&data.variants)?;
                let meta = ReflectMeta::new(&input.ident, &input.generics, traits);

                let reflect_enum = ReflectEnum {
                    meta,
                    variants,
                    remote_ty: None,
                };
                Ok(Self::Enum(reflect_enum))
            }
            Data::Union(..) => Err(syn::Error::new(
                input.span(),
                "reflection not supported for unions",
            )),
        };
    }

    /// Set the remote type for this derived type.
    ///
    /// # Panics
    ///
    /// Panics when called on [`ReflectDerive::Value`].
    pub fn set_remote(&mut self, remote_ty: Option<&'a TypePath>) {
        match self {
            Self::Struct(data) | Self::TupleStruct(data) | Self::UnitStruct(data) => {
                data.remote_ty = remote_ty;
            }
            Self::Enum(data) => {
                data.remote_ty = remote_ty;
            }
            _ => panic!("cannot create a reflected value type for a remote type"),
        }
    }

    fn collect_struct_fields(fields: &'a Fields) -> Result<Vec<StructField<'a>>, syn::Error> {
        let sifter: utility::ResultSifter<StructField<'a>> = fields
            .iter()
            .enumerate()
            .map(|(index, field)| -> Result<StructField, syn::Error> {
                let attrs = parse_field_attrs(&field.attrs)?;
                Ok(StructField {
                    index,
                    attrs,
                    data: field,
                })
            })
            .fold(
                utility::ResultSifter::default(),
                utility::ResultSifter::fold,
            );

        sifter.finish()
    }

    fn collect_enum_variants(
        variants: &'a Punctuated<Variant, Token![,]>,
    ) -> Result<Vec<EnumVariant<'a>>, syn::Error> {
        let sifter: utility::ResultSifter<EnumVariant<'a>> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| -> Result<EnumVariant, syn::Error> {
                let fields = Self::collect_struct_fields(&variant.fields)?;

                let fields = match variant.fields {
                    Fields::Named(..) => EnumVariantFields::Named(fields),
                    Fields::Unnamed(..) => EnumVariantFields::Unnamed(fields),
                    Fields::Unit => EnumVariantFields::Unit,
                };
                Ok(EnumVariant {
                    fields,
                    attrs: parse_field_attrs(&variant.attrs)?,
                    data: variant,
                    index,
                })
            })
            .fold(
                utility::ResultSifter::default(),
                utility::ResultSifter::fold,
            );

        sifter.finish()
    }
}

impl<'a> ReflectMeta<'a> {
    pub fn new(type_name: &'a Ident, generics: &'a Generics, traits: ReflectTraits) -> Self {
        Self {
            traits,
            type_name,
            generics,
            bevy_reflect_path: utility::get_bevy_reflect_path(),
        }
    }

    /// The registered reflect traits on this struct.
    pub fn traits(&self) -> &ReflectTraits {
        &self.traits
    }

    /// The name of this struct.
    pub fn type_name(&self) -> &'a Ident {
        self.type_name
    }

    /// The generics associated with this struct.
    pub fn generics(&self) -> &'a Generics {
        self.generics
    }

    /// The cached `bevy_reflect` path.
    pub fn bevy_reflect_path(&self) -> &Path {
        &self.bevy_reflect_path
    }

    /// Returns the `GetTypeRegistration` impl as a `TokenStream`.
    pub fn get_type_registration(&self) -> proc_macro2::TokenStream {
        crate::registration::impl_get_type_registration(
            self.type_name,
            &self.bevy_reflect_path,
            self.traits.idents(),
            self.generics,
            None,
        )
    }
}

impl<'a> StructField<'a> {
    /// Returns the reflected type of this field.
    ///
    /// Normally this is just the field's defined type.
    /// However, this can be adjusted to use a different type, like for representing remote types.
    /// In those cases, the returned value is the remote wrapper type.
    pub fn reflected_type(&self) -> &Type {
        self.attrs.remote.as_ref().unwrap_or(&self.data.ty)
    }
}

impl<'a> ReflectStruct<'a> {
    /// Access the metadata associated with this struct definition.
    pub fn meta(&self) -> &ReflectMeta<'a> {
        &self.meta
    }

    /// Whether this reflected struct represents a remote type or not.
    pub fn is_remote(&self) -> bool {
        self.remote_ty.is_some()
    }

    #[allow(dead_code)]
    /// Get the remote type path, if any.
    pub fn remote_ty(&self) -> Option<&'a TypePath> {
        self.remote_ty
    }

    /// Access the data about which fields should be ignored during serialization.
    ///
    /// The returned bitset is a collection of indices obtained from the [`members_to_serialization_denylist`](crate::utility::members_to_serialization_denylist) function.
    #[allow(dead_code)]
    pub fn serialization_denylist(&self) -> &BitSet<u32> {
        &self.serialization_denylist
    }

    /// Returns the `GetTypeRegistration` impl as a `TokenStream`.
    ///
    /// Returns a specific implementation for structs and this method should be preffered over the generic [`get_type_registration`](crate::ReflectMeta) method
    pub fn get_type_registration(&self) -> proc_macro2::TokenStream {
        let reflect_path = self.meta.bevy_reflect_path();

        crate::registration::impl_get_type_registration(
            self.meta.type_name(),
            reflect_path,
            self.meta.traits().idents(),
            self.meta.generics(),
            Some(&self.serialization_denylist),
        )
    }

    /// Get a collection of types which are exposed to the reflection API
    pub fn active_types(&self) -> Vec<syn::Type> {
        self.fields
            .iter()
            .filter(move |field| field.attrs.ignore.is_active())
            .map(|field| field.reflected_type().clone())
            .collect::<Vec<_>>()
    }

    /// Get an iterator of fields which are exposed to the reflection API
    pub fn active_fields(&self) -> impl Iterator<Item = &StructField<'a>> {
        self.fields
            .iter()
            .filter(move |field| field.attrs.ignore.is_active())
    }

    /// Get an iterator of fields which are ignored by the reflection API
    pub fn ignored_fields(&self) -> impl Iterator<Item = &StructField<'a>> {
        self.fields
            .iter()
            .filter(move |field| field.attrs.ignore.is_ignored())
    }

    /// The complete set of fields in this struct.
    #[allow(dead_code)]
    pub fn fields(&self) -> &[StructField<'a>] {
        &self.fields
    }
}

impl<'a> ReflectEnum<'a> {
    /// Access the metadata associated with this enum definition.
    pub fn meta(&self) -> &ReflectMeta<'a> {
        &self.meta
    }

    /// Whether this reflected enum represents a remote type or not.
    pub fn is_remote(&self) -> bool {
        self.remote_ty.is_some()
    }

    #[allow(dead_code)]
    /// Get the remote type path, if any.
    pub fn remote_ty(&self) -> Option<&'a TypePath> {
        self.remote_ty
    }

    /// Returns the given ident as a qualified unit variant of this enum.
    ///
    /// This takes into account the remote type, if any.
    pub fn get_unit(&self, variant: &Ident) -> proc_macro2::TokenStream {
        let name = self
            .remote_ty
            .map(|path| path.to_token_stream())
            .unwrap_or_else(|| self.meta.type_name.to_token_stream());

        quote! {
            #name::#variant
        }
    }

    /// The complete set of variants in this enum.
    pub fn variants(&self) -> &[EnumVariant<'a>] {
        &self.variants
    }
}
