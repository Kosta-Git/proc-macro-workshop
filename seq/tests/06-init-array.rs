// This test case should hopefully be a freebie if all of the previous ones are
// passing. This test demonstrates using the seq macro to construct a const
// array literal.
//
// The generated code would be:
//
//     [Proc::new(0), Proc::new(1), ..., Proc::new(255),]

use seq::seq;

const PROCS: [Proc; 256] = {
    seq!(N in 0..256 {
        [
            #(
                Proc::new(
                    N,
                    [
                        #(
                            N,
                        )*
                    ]
                ),
            )*
        ]
    })
};

struct Proc {
    id: usize,
    arr: [u32; 256],
}

impl Proc {
    const fn new(id: usize, arr: [u32; 256]) -> Self {
        Proc { id, arr }
    }
}

fn main() {
    assert_eq!(PROCS[32].id, 32);
    assert_eq!(PROCS[32].arr[32], 32);
}
