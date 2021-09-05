# Interruptible sleep in Rust

In my projects, I've often wanted to put a thread to sleep for some fixed duration, such that it can be woken up early by another thread.

My first instinct was to raise a Unix signal and catch the resulting `EINTR` in the receiving thread. But the `sleep()` function in Rust loops on `EINTR` by default; and even if it didn't, this solution still wouldn't be portable.

The solution I settled on uses [`std::thread::park_timeout()`][park_timeout]. This function blocks until another thread unblocks it, or some amount of time has passed, whichever comes first. This behavior is perfect for my use case.

Here's an example that uses `park_timeout()` to implement a polling loop. Along with an atomic "keep going" flag, this allows for stopping a worker thread in a controlled way.

```rust
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;

fn main() {
    let keep_going = Arc::new(AtomicBool::new(true));
    let worker = {
        let keep_going = keep_going.clone();
        thread::spawn(move || loop {
            if !keep_going.load(Ordering::SeqCst) { break; }
            thread::park_timeout(Duration::from_secs(5));
            if !keep_going.load(Ordering::SeqCst) { break; }
            println!("expensive processing begin!!");
            // An expensive operation that can't be cancelled
            thread::sleep(Duration::from_secs(2));
            println!("expensive processing finish!!!");
        })
    };

    println!("press <enter> to stop");
    let mut dummy = String::new();
    std::io::stdin().read_line(&mut dummy).unwrap();

    keep_going.store(false, Ordering::SeqCst);
    worker.thread().unpark();

    println!("waiting for worker to finish");
    worker.join().unwrap();

    println!("bye!!! ^_^");
}

```

[park_timeout]: https://doc.rust-lang.org/std/thread/fn.park_timeout.html
