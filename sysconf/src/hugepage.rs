#[cfg(target_os = "linux")]
pub fn hugepage_supported(size: usize) -> bool {
    // Since the hugepages directories are of the form hugepages-${size}kB, we're guaranteed that,
    // barring a huge change, Linux will never support hugepages smaller than 1kB. Since this
    // interface replaces the /proc/meminfo interface, and thus must support expressing anything
    // that that interface can express, we also know that /proc/meminfo cannot support hugepages
    // smaller than 1kB. Thus, this 'size < 1024' check can never produce a false positive.
    if size < 1024 || !size.is_power_of_two() {
        return false;
    }

    #[cfg(target_pointer_width = "32")]
    match size.trailing_zeros() {
        10 => *lazy::HUGEPAGE_SUPPORTED_10,
        11 => *lazy::HUGEPAGE_SUPPORTED_11,
        12 => *lazy::HUGEPAGE_SUPPORTED_12,
        13 => *lazy::HUGEPAGE_SUPPORTED_13,
        14 => *lazy::HUGEPAGE_SUPPORTED_14,
        15 => *lazy::HUGEPAGE_SUPPORTED_15,
        16 => *lazy::HUGEPAGE_SUPPORTED_16,
        17 => *lazy::HUGEPAGE_SUPPORTED_17,
        18 => *lazy::HUGEPAGE_SUPPORTED_18,
        19 => *lazy::HUGEPAGE_SUPPORTED_19,
        20 => *lazy::HUGEPAGE_SUPPORTED_20,
        21 => *lazy::HUGEPAGE_SUPPORTED_21,
        22 => *lazy::HUGEPAGE_SUPPORTED_22,
        23 => *lazy::HUGEPAGE_SUPPORTED_23,
        24 => *lazy::HUGEPAGE_SUPPORTED_24,
        25 => *lazy::HUGEPAGE_SUPPORTED_25,
        26 => *lazy::HUGEPAGE_SUPPORTED_26,
        27 => *lazy::HUGEPAGE_SUPPORTED_27,
        28 => *lazy::HUGEPAGE_SUPPORTED_28,
        29 => *lazy::HUGEPAGE_SUPPORTED_29,
        30 => *lazy::HUGEPAGE_SUPPORTED_30,
        31 => *lazy::HUGEPAGE_SUPPORTED_31,
        _ => unreachable!(),
    }

    #[cfg(target_pointer_width = "64")]
    match size.trailing_zeros() {
        10 => *lazy::HUGEPAGE_SUPPORTED_10,
        11 => *lazy::HUGEPAGE_SUPPORTED_11,
        12 => *lazy::HUGEPAGE_SUPPORTED_12,
        13 => *lazy::HUGEPAGE_SUPPORTED_13,
        14 => *lazy::HUGEPAGE_SUPPORTED_14,
        15 => *lazy::HUGEPAGE_SUPPORTED_15,
        16 => *lazy::HUGEPAGE_SUPPORTED_16,
        17 => *lazy::HUGEPAGE_SUPPORTED_17,
        18 => *lazy::HUGEPAGE_SUPPORTED_18,
        19 => *lazy::HUGEPAGE_SUPPORTED_19,
        20 => *lazy::HUGEPAGE_SUPPORTED_20,
        21 => *lazy::HUGEPAGE_SUPPORTED_21,
        22 => *lazy::HUGEPAGE_SUPPORTED_22,
        23 => *lazy::HUGEPAGE_SUPPORTED_23,
        24 => *lazy::HUGEPAGE_SUPPORTED_24,
        25 => *lazy::HUGEPAGE_SUPPORTED_25,
        26 => *lazy::HUGEPAGE_SUPPORTED_26,
        27 => *lazy::HUGEPAGE_SUPPORTED_27,
        28 => *lazy::HUGEPAGE_SUPPORTED_28,
        29 => *lazy::HUGEPAGE_SUPPORTED_29,
        30 => *lazy::HUGEPAGE_SUPPORTED_30,
        31 => *lazy::HUGEPAGE_SUPPORTED_31,
        32 => *lazy::HUGEPAGE_SUPPORTED_32,
        33 => *lazy::HUGEPAGE_SUPPORTED_33,
        34 => *lazy::HUGEPAGE_SUPPORTED_34,
        35 => *lazy::HUGEPAGE_SUPPORTED_35,
        36 => *lazy::HUGEPAGE_SUPPORTED_36,
        37 => *lazy::HUGEPAGE_SUPPORTED_37,
        38 => *lazy::HUGEPAGE_SUPPORTED_38,
        39 => *lazy::HUGEPAGE_SUPPORTED_39,
        40 => *lazy::HUGEPAGE_SUPPORTED_40,
        41 => *lazy::HUGEPAGE_SUPPORTED_41,
        42 => *lazy::HUGEPAGE_SUPPORTED_42,
        43 => *lazy::HUGEPAGE_SUPPORTED_43,
        44 => *lazy::HUGEPAGE_SUPPORTED_44,
        45 => *lazy::HUGEPAGE_SUPPORTED_45,
        46 => *lazy::HUGEPAGE_SUPPORTED_46,
        47 => *lazy::HUGEPAGE_SUPPORTED_47,
        48 => *lazy::HUGEPAGE_SUPPORTED_48,
        49 => *lazy::HUGEPAGE_SUPPORTED_49,
        50 => *lazy::HUGEPAGE_SUPPORTED_50,
        51 => *lazy::HUGEPAGE_SUPPORTED_51,
        52 => *lazy::HUGEPAGE_SUPPORTED_52,
        53 => *lazy::HUGEPAGE_SUPPORTED_53,
        54 => *lazy::HUGEPAGE_SUPPORTED_54,
        55 => *lazy::HUGEPAGE_SUPPORTED_55,
        56 => *lazy::HUGEPAGE_SUPPORTED_56,
        57 => *lazy::HUGEPAGE_SUPPORTED_57,
        58 => *lazy::HUGEPAGE_SUPPORTED_58,
        59 => *lazy::HUGEPAGE_SUPPORTED_59,
        60 => *lazy::HUGEPAGE_SUPPORTED_60,
        61 => *lazy::HUGEPAGE_SUPPORTED_61,
        62 => *lazy::HUGEPAGE_SUPPORTED_62,
        63 => *lazy::HUGEPAGE_SUPPORTED_63,
        _ => unreachable!(),
    }
}

#[cfg(all(target_os = "linux", target_pointer_width = "31"))]
mod lazy {
    use super::priv_hugepage_supported;
    // Doing this all in a single lazy_static block causes the macro expansion pass to exceed its
    // recursion limit. This is ugly, but it works. It's also the reason that this is in its own
    // module - so that the single #[cfg] directive can apply to the whole module rather than
    // having to repeat it for each lazy_static block.
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_10: bool = priv_hugepage_supported(10); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_11: bool = priv_hugepage_supported(11); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_12: bool = priv_hugepage_supported(12); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_13: bool = priv_hugepage_supported(13); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_14: bool = priv_hugepage_supported(14); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_15: bool = priv_hugepage_supported(15); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_16: bool = priv_hugepage_supported(16); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_17: bool = priv_hugepage_supported(17); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_18: bool = priv_hugepage_supported(18); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_19: bool = priv_hugepage_supported(19); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_20: bool = priv_hugepage_supported(20); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_21: bool = priv_hugepage_supported(21); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_22: bool = priv_hugepage_supported(22); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_23: bool = priv_hugepage_supported(23); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_24: bool = priv_hugepage_supported(24); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_25: bool = priv_hugepage_supported(25); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_26: bool = priv_hugepage_supported(26); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_27: bool = priv_hugepage_supported(27); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_28: bool = priv_hugepage_supported(28); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_29: bool = priv_hugepage_supported(29); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_30: bool = priv_hugepage_supported(30); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_31: bool = priv_hugepage_supported(31); }
}

#[cfg(all(target_os = "linux", target_pointer_width = "64"))]
mod lazy {
    use super::priv_hugepage_supported;
    // Doing this all in a single lazy_static block causes the macro expansion pass to exceed its
    // recursion limit. This is ugly, but it works. It's also the reason that this is in its own
    // module - so that the single #[cfg] directive can apply to the whole module rather than
    // having to repeat it for each lazy_static block.
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_10: bool = priv_hugepage_supported(10); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_11: bool = priv_hugepage_supported(11); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_12: bool = priv_hugepage_supported(12); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_13: bool = priv_hugepage_supported(13); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_14: bool = priv_hugepage_supported(14); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_15: bool = priv_hugepage_supported(15); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_16: bool = priv_hugepage_supported(16); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_17: bool = priv_hugepage_supported(17); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_18: bool = priv_hugepage_supported(18); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_19: bool = priv_hugepage_supported(19); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_20: bool = priv_hugepage_supported(20); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_21: bool = priv_hugepage_supported(21); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_22: bool = priv_hugepage_supported(22); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_23: bool = priv_hugepage_supported(23); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_24: bool = priv_hugepage_supported(24); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_25: bool = priv_hugepage_supported(25); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_26: bool = priv_hugepage_supported(26); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_27: bool = priv_hugepage_supported(27); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_28: bool = priv_hugepage_supported(28); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_29: bool = priv_hugepage_supported(29); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_30: bool = priv_hugepage_supported(30); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_31: bool = priv_hugepage_supported(31); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_32: bool = priv_hugepage_supported(32); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_33: bool = priv_hugepage_supported(33); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_34: bool = priv_hugepage_supported(34); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_35: bool = priv_hugepage_supported(35); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_36: bool = priv_hugepage_supported(36); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_37: bool = priv_hugepage_supported(37); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_38: bool = priv_hugepage_supported(38); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_39: bool = priv_hugepage_supported(39); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_40: bool = priv_hugepage_supported(40); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_41: bool = priv_hugepage_supported(41); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_42: bool = priv_hugepage_supported(42); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_43: bool = priv_hugepage_supported(43); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_44: bool = priv_hugepage_supported(44); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_45: bool = priv_hugepage_supported(45); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_46: bool = priv_hugepage_supported(46); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_47: bool = priv_hugepage_supported(47); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_48: bool = priv_hugepage_supported(48); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_49: bool = priv_hugepage_supported(49); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_50: bool = priv_hugepage_supported(50); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_51: bool = priv_hugepage_supported(51); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_52: bool = priv_hugepage_supported(52); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_53: bool = priv_hugepage_supported(53); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_54: bool = priv_hugepage_supported(54); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_55: bool = priv_hugepage_supported(55); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_56: bool = priv_hugepage_supported(56); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_57: bool = priv_hugepage_supported(57); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_58: bool = priv_hugepage_supported(58); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_59: bool = priv_hugepage_supported(59); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_60: bool = priv_hugepage_supported(60); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_61: bool = priv_hugepage_supported(61); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_62: bool = priv_hugepage_supported(62); }
    lazy_static!{ pub static ref HUGEPAGE_SUPPORTED_63: bool = priv_hugepage_supported(63); }
}

#[cfg(target_os = "linux")]
macro_rules! get_linux_hugepage_directory {
    (32, $size:expr) => (
        match $size {
            10 => "/sys/kernel/mm/hugepages/hugepages-1kB",
            11 => "/sys/kernel/mm/hugepages/hugepages-2kB",
            12 => "/sys/kernel/mm/hugepages/hugepages-4kB",
            13 => "/sys/kernel/mm/hugepages/hugepages-8kB",
            14 => "/sys/kernel/mm/hugepages/hugepages-16kB",
            15 => "/sys/kernel/mm/hugepages/hugepages-32kB",
            16 => "/sys/kernel/mm/hugepages/hugepages-64kB",
            17 => "/sys/kernel/mm/hugepages/hugepages-128kB",
            18 => "/sys/kernel/mm/hugepages/hugepages-256kB",
            19 => "/sys/kernel/mm/hugepages/hugepages-512kB",
            20 => "/sys/kernel/mm/hugepages/hugepages-1024kB",
            21 => "/sys/kernel/mm/hugepages/hugepages-2048kB",
            22 => "/sys/kernel/mm/hugepages/hugepages-4096kB",
            23 => "/sys/kernel/mm/hugepages/hugepages-8192kB",
            24 => "/sys/kernel/mm/hugepages/hugepages-16384kB",
            25 => "/sys/kernel/mm/hugepages/hugepages-32768kB",
            26 => "/sys/kernel/mm/hugepages/hugepages-65536kB",
            27 => "/sys/kernel/mm/hugepages/hugepages-131072kB",
            28 => "/sys/kernel/mm/hugepages/hugepages-262144kB",
            29 => "/sys/kernel/mm/hugepages/hugepages-524288kB",
            30 => "/sys/kernel/mm/hugepages/hugepages-1048576kB",
            31 => "/sys/kernel/mm/hugepages/hugepages-2097152kB",
            _ => unreachable!(),
        }
    );
    (64, $size:expr) => (
        match $size {
            10 => "/sys/kernel/mm/hugepages/hugepages-1kB",
            11 => "/sys/kernel/mm/hugepages/hugepages-2kB",
            12 => "/sys/kernel/mm/hugepages/hugepages-4kB",
            13 => "/sys/kernel/mm/hugepages/hugepages-8kB",
            14 => "/sys/kernel/mm/hugepages/hugepages-16kB",
            15 => "/sys/kernel/mm/hugepages/hugepages-32kB",
            16 => "/sys/kernel/mm/hugepages/hugepages-64kB",
            17 => "/sys/kernel/mm/hugepages/hugepages-128kB",
            18 => "/sys/kernel/mm/hugepages/hugepages-256kB",
            19 => "/sys/kernel/mm/hugepages/hugepages-512kB",
            20 => "/sys/kernel/mm/hugepages/hugepages-1024kB",
            21 => "/sys/kernel/mm/hugepages/hugepages-2048kB",
            22 => "/sys/kernel/mm/hugepages/hugepages-4096kB",
            23 => "/sys/kernel/mm/hugepages/hugepages-8192kB",
            24 => "/sys/kernel/mm/hugepages/hugepages-16384kB",
            25 => "/sys/kernel/mm/hugepages/hugepages-32768kB",
            26 => "/sys/kernel/mm/hugepages/hugepages-65536kB",
            27 => "/sys/kernel/mm/hugepages/hugepages-131072kB",
            28 => "/sys/kernel/mm/hugepages/hugepages-262144kB",
            29 => "/sys/kernel/mm/hugepages/hugepages-524288kB",
            30 => "/sys/kernel/mm/hugepages/hugepages-1048576kB",
            31 => "/sys/kernel/mm/hugepages/hugepages-2097152kB",
            32 => "/sys/kernel/mm/hugepages/hugepages-4194304kB",
            33 => "/sys/kernel/mm/hugepages/hugepages-8388608kB",
            34 => "/sys/kernel/mm/hugepages/hugepages-16777216kB",
            35 => "/sys/kernel/mm/hugepages/hugepages-33554432kB",
            36 => "/sys/kernel/mm/hugepages/hugepages-67108864kB",
            37 => "/sys/kernel/mm/hugepages/hugepages-134217728kB",
            38 => "/sys/kernel/mm/hugepages/hugepages-268435456kB",
            39 => "/sys/kernel/mm/hugepages/hugepages-536870912kB",
            40 => "/sys/kernel/mm/hugepages/hugepages-1073741824kB",
            41 => "/sys/kernel/mm/hugepages/hugepages-2147483648kB",
            42 => "/sys/kernel/mm/hugepages/hugepages-4294967296kB",
            43 => "/sys/kernel/mm/hugepages/hugepages-8589934592kB",
            44 => "/sys/kernel/mm/hugepages/hugepages-17179869184kB",
            45 => "/sys/kernel/mm/hugepages/hugepages-34359738368kB",
            46 => "/sys/kernel/mm/hugepages/hugepages-68719476736kB",
            47 => "/sys/kernel/mm/hugepages/hugepages-137438953472kB",
            48 => "/sys/kernel/mm/hugepages/hugepages-274877906944kB",
            49 => "/sys/kernel/mm/hugepages/hugepages-549755813888kB",
            50 => "/sys/kernel/mm/hugepages/hugepages-1099511627776kB",
            51 => "/sys/kernel/mm/hugepages/hugepages-2199023255552kB",
            52 => "/sys/kernel/mm/hugepages/hugepages-4398046511104kB",
            53 => "/sys/kernel/mm/hugepages/hugepages-8796093022208kB",
            54 => "/sys/kernel/mm/hugepages/hugepages-17592186044416kB",
            55 => "/sys/kernel/mm/hugepages/hugepages-35184372088832kB",
            56 => "/sys/kernel/mm/hugepages/hugepages-70368744177664kB",
            57 => "/sys/kernel/mm/hugepages/hugepages-140737488355328kB",
            58 => "/sys/kernel/mm/hugepages/hugepages-281474976710656kB",
            59 => "/sys/kernel/mm/hugepages/hugepages-562949953421312kB",
            60 => "/sys/kernel/mm/hugepages/hugepages-1125899906842624kB",
            61 => "/sys/kernel/mm/hugepages/hugepages-2251799813685248kB",
            62 => "/sys/kernel/mm/hugepages/hugepages-4503599627370496kB",
            63 => "/sys/kernel/mm/hugepages/hugepages-9007199254740992kB",
            _ => unreachable!(),
        }
    );
}

#[cfg(target_os = "linux")]
fn priv_hugepage_supported(exp: usize) -> bool {
    // See for details: https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
    // First, use the more modern method of checking /sys/kernel/mm/hugepages/hugepages-${size}kB.
    // If that fails (possibly because we're on an old kernel version), try the legacy method of
    // parsing /proc/meminfo (described in more detail in legacy_hugepage_supported below).

    use libc::{stat, lstat, ENOENT, ENOMEM};
    use errno::errno;
    use core::mem::uninitialized;

    #[cfg(target_pointer_width = "32")]
    let path = get_linux_hugepage_directory!(32, exp);
    #[cfg(target_pointer_width = "64")]
    let path = get_linux_hugepage_directory!(64, exp);

    let mut s = unsafe { uninitialized::<stat>() };
    if unsafe { lstat(path.as_ptr() as *const i8, &mut s) } < 0 {
        // No other error should be possible here (see man 2 lstat)
        let e = errno().0;
        assert!(e == ENOENT || e == ENOMEM);
        if e == ENOENT {
            // Maybe we're on an older kernel that doesn't support /sys/kernel/mm/hugepages;
            // it will still support /proc/meminfo, which is what default_hugepage uses.
            default_hugepage() == Some(1 << exp)
        } else {
            false
        }
    } else {
        true
    }
}

#[cfg(any(target_os = "linux", windows))]
pub fn default_hugepage() -> Option<usize> {
    *DEFAULT_HUGEPAGE
}

#[cfg(any(target_os = "linux", windows))]
lazy_static!{ static ref DEFAULT_HUGEPAGE: Option<usize> = priv_default_hugepage(); }

#[cfg(target_os = "linux")]
fn priv_default_hugepage() -> Option<usize> {
    // Parse /proc/meminfo looking for the line 'Hugepagesize: xxx kB'.
    // TODO: Implement
    None
}

#[cfg(windows)]
fn priv_default_hugepage() -> Option<usize> {
    use kernel32::GetLargePageMinimum;
    unsafe {
        let size = GetLargePageMinimum();
        // While u64 might be larger than usize (on 32-bit systems), if 'size' were to overflow
        // usize (and thus be larger than 2^32), that would imply that the huge page size was too
        // large for the address of the second huge page on the system to be representable with a
        // pointer. Obviously that wouldn't happen, so we don't bother to check for overflow.
        if size == 0 { None } else { Some(size as usize) }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[cfg(target_os = "linux")]
    fn test_hugepage_supported() {
        use core::usize::MAX;
        use std::fs::metadata;
        use hugepage::hugepage_supported;

        let max = MAX - (MAX >> 1); // largest power of two representable by usize
        let mut size = 512; // start off at 512 so it will be 1024 in the first loop iteration
        // this has the effect of letting size <= max after the 'size *= 2' line
        while size < max {
            size *= 2;
            #[cfg(target_pointer_width = "32")]
            let path = get_linux_hugepage_directory!(32, size.trailing_zeros());
            #[cfg(target_pointer_width = "64")]
            let path = get_linux_hugepage_directory!(64, size.trailing_zeros());

            let supported = match metadata(path) {
                Ok(_) => true,
                Err(_) => false,
            };

            assert_eq!(supported, hugepage_supported(size));
        }
    }
}
