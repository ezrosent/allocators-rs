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
