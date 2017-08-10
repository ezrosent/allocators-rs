//! Types of various sizes for testing `ObjectAlloc`s.
//!
//! This module defines a number of types ranging in size from 1 through 2^13 = 8192 bytes. The
//! sizes used include all powers of two, all midpoints between successive powers of two (that is,
//! `x + x/2` where `x` is a power of two), and at least one prime in between any two even sizes.
//! Each type is named `ByteN` where `N` is the type's size in bytes.

/// Invoke a macro on multiple sets of arguments.
///
/// `call_macro!` invokes the macro `m` on 0 or more sets of arguments. Each set of arguments must
/// be surrounded in parentheses.
///
/// # Examples
///
/// ```rust
/// # #[macro_use] extern crate object_alloc_test;
/// # fn main() {
/// call_macro!(println, ("Once upon a midnight dreary, while I pondered, weak and weary,"),
///                      ("Over many a quaint and curious volume of forgotten lore—"),
///                      ("While I nodded, nearly napping, suddenly there came a tapping,"),
///                      ("As of some one gently rapping, rapping at my chamber door."),
///                      ("\"'Tis some visiter,\" I muttered, \"tapping at my chamber door—"),
///                      ("            Only this and nothing more."));
/// # }
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! call_macro {
    ($m:ident, $($arg:tt),*) => {
        $(
            $m! $arg ;
        )*
    }
}

macro_rules! impl_byte_n {
    ($type:ident, $n:tt) => (
        pub struct $type(pub [u8; $n]);
        impl Default for $type {
            fn default() -> $type {
                $type([0; $n])
            }
        }
    )
}

call_macro!(impl_byte_n,
            (Byte1, 1),
            (Byte2, 2),
            (Byte3, 3),
            (Byte4, 4),
            (Byte5, 5),
            (Byte6, 6),
            (Byte7, 7),
            (Byte8, 8),
            (Byte11, 11),
            (Byte12, 12),
            (Byte13, 13),
            (Byte16, 16),
            (Byte19, 19),
            (Byte24, 24),
            (Byte29, 29),
            (Byte32, 32),
            (Byte41, 41),
            (Byte48, 48),
            (Byte59, 59),
            (Byte64, 64),
            (Byte73, 73),
            (Byte96, 96),
            (Byte113, 113),
            (Byte128, 128),
            (Byte157, 157),
            (Byte192, 192),
            (Byte229, 229),
            (Byte256, 256),
            (Byte317, 317),
            (Byte384, 384),
            (Byte457, 457),
            (Byte512, 512),
            (Byte768, 768),
            (Byte617, 617),
            (Byte1024, 1024),
            (Byte1277, 1277),
            (Byte1536, 1536),
            (Byte1777, 1777),
            (Byte2048, 2048),
            (Byte2557, 2557),
            (Byte3072, 3072),
            (Byte3539, 3539),
            (Byte4096, 4096),
            (Byte5119, 5119),
            (Byte6144, 6144),
            (Byte7151, 7151),
            (Byte8192, 8192));

// TODO: Document the pattern of matching particular types to avoid defining anything for them.

/// Call a macro once for each type defined in the `types` module.
///
/// `call_for_all_types_prefix` is useful for defining something (usually test functions) once for
/// each type defined in the `test` module. The first argument, `fn`, is the name of a macro to
/// invoke. This macro will be invoked once for each type with its first argument being a
/// constructed identifier and the second argument being the type. For example,
/// `call_for_all_types_prefix!(foo, bar)` expands to:
///
/// ```rust,ignore
/// foo!(bar_0001_byte, $crate::types::Byte1);
/// foo!(bar_0002_byte, $crate::types::Byte2);
/// foo!(bar_0003_byte, $crate::types::Byte3);
/// // ...etc
/// ```
///
/// An arbitrary number of optional arguments may also be supplied; these will be
/// passed as additional trailing arguments in the invocation of the macro. For example,
/// `call_for_all_types_prefix!(foo, bar, baz, blah)` expands to:
///
/// ```rust,ignore
/// foo!(bar_0001_byte, $crate::types::Byte1, baz, blah);
/// foo!(bar_0002_byte, $crate::types::Byte2, baz, blah);
/// foo!(bar_0003_byte, $crate::types::Byte3, baz, blah);
/// // ...etc
/// ```
///
/// # Examples
///
/// ```rust
/// # #![feature(plugin)]
/// # #![plugin(interpolate_idents)]
/// # #[macro_use] extern crate object_alloc_test;
/// # fn main() {
/// macro_rules! make_default_test {
///     ($name:ident, $type:ty) => (
///         fn $name() {
///             <$type>::default();
///         }
///     )
/// }
///
/// call_for_all_types_prefix!(make_default_test, default_test);
/// # }
/// ```
#[macro_export]
macro_rules! call_for_all_types_prefix {
    // NOTE: The '$(, $arg:tt)*' syntax defines a set of optional arguments (note that there's only
    // a comma following '$prefix:ident' if there are optional arguments)
    ($fn:ident, $prefix:ident $(, $arg:tt)*) => (
        interpolate_idents! {
            call_macro!($fn,
                        ([$prefix _0001_byte], $crate::types::Byte1 $(,$arg)*),
                        ([$prefix _0002_byte], $crate::types::Byte2 $(,$arg)*),
                        ([$prefix _0003_byte], $crate::types::Byte3 $(,$arg)*),
                        ([$prefix _0004_byte], $crate::types::Byte4 $(,$arg)*),
                        ([$prefix _0005_byte], $crate::types::Byte5 $(,$arg)*),
                        ([$prefix _0006_byte], $crate::types::Byte6 $(,$arg)*),
                        ([$prefix _0007_byte], $crate::types::Byte7 $(,$arg)*),
                        ([$prefix _0008_byte], $crate::types::Byte8 $(,$arg)*),
                        ([$prefix _0011_byte], $crate::types::Byte11 $(,$arg)*),
                        ([$prefix _0012_byte], $crate::types::Byte12 $(,$arg)*),
                        ([$prefix _0013_byte], $crate::types::Byte13 $(,$arg)*),
                        ([$prefix _0016_byte], $crate::types::Byte16 $(,$arg)*),
                        ([$prefix _0019_byte], $crate::types::Byte19 $(,$arg)*),
                        ([$prefix _0024_byte], $crate::types::Byte24 $(,$arg)*),
                        ([$prefix _0029_byte], $crate::types::Byte29 $(,$arg)*),
                        ([$prefix _0032_byte], $crate::types::Byte32 $(,$arg)*),
                        ([$prefix _0041_byte], $crate::types::Byte41 $(,$arg)*),
                        ([$prefix _0048_byte], $crate::types::Byte48 $(,$arg)*),
                        ([$prefix _0059_byte], $crate::types::Byte59 $(,$arg)*),
                        ([$prefix _0064_byte], $crate::types::Byte64 $(,$arg)*),
                        ([$prefix _0073_byte], $crate::types::Byte73 $(,$arg)*),
                        ([$prefix _0096_byte], $crate::types::Byte96 $(,$arg)*),
                        ([$prefix _0113_byte], $crate::types::Byte113 $(,$arg)*),
                        ([$prefix _0128_byte], $crate::types::Byte128 $(,$arg)*),
                        ([$prefix _0157_byte], $crate::types::Byte157 $(,$arg)*),
                        ([$prefix _0192_byte], $crate::types::Byte192 $(,$arg)*),
                        ([$prefix _0229_byte], $crate::types::Byte229 $(,$arg)*),
                        ([$prefix _0256_byte], $crate::types::Byte256 $(,$arg)*),
                        ([$prefix _0317_byte], $crate::types::Byte317 $(,$arg)*),
                        ([$prefix _0384_byte], $crate::types::Byte384 $(,$arg)*),
                        ([$prefix _0457_byte], $crate::types::Byte457 $(,$arg)*),
                        ([$prefix _0512_byte], $crate::types::Byte512 $(,$arg)*),
                        ([$prefix _0768_byte], $crate::types::Byte768 $(,$arg)*),
                        ([$prefix _0617_byte], $crate::types::Byte617 $(,$arg)*),
                        ([$prefix _1024_byte], $crate::types::Byte1024 $(,$arg)*),
                        ([$prefix _1277_byte], $crate::types::Byte1277 $(,$arg)*),
                        ([$prefix _1536_byte], $crate::types::Byte1536 $(,$arg)*),
                        ([$prefix _1777_byte], $crate::types::Byte1777 $(,$arg)*),
                        ([$prefix _2048_byte], $crate::types::Byte2048 $(,$arg)*),
                        ([$prefix _2557_byte], $crate::types::Byte2557 $(,$arg)*),
                        ([$prefix _3072_byte], $crate::types::Byte3072 $(,$arg)*),
                        ([$prefix _3539_byte], $crate::types::Byte3539 $(,$arg)*),
                        ([$prefix _4096_byte], $crate::types::Byte4096 $(,$arg)*),
                        ([$prefix _5119_byte], $crate::types::Byte5119 $(,$arg)*),
                        ([$prefix _6144_byte], $crate::types::Byte6144 $(,$arg)*),
                        ([$prefix _7151_byte], $crate::types::Byte7151 $(,$arg)*),
                        ([$prefix _8192_byte], $crate::types::Byte8192 $(,$arg)*));
        }
    );
}
