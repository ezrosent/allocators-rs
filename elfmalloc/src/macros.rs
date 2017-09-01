// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

#[allow(unused)]
macro_rules! println_stderr {
    // courtesy joshlf
    () => ({use std; use std::io::Write; let _ = writeln!(&mut std::io::stderr());});
    ($fmt:expr) => ({use std; use std::io::Write; let _ = writeln!(&mut std::io::stderr(), $fmt);});
    ($fmt:expr, $($arg:tt)*) => ({
            use std;
            use std::io::Write;
            let _ = writeln!(&mut std::io::stderr(), $fmt, $($arg)*);
        });
}

#[macro_export]
macro_rules! dbg_print {
    ($fmt:expr, $($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println_stderr!($fmt, $($arg)*);
        }
    }
}
