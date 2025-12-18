use seq::seq;

seq!(N in 0..256 {
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Interrupt {
        #(
            Irq~N,
        )*
    }
});

struct Proc {
    id: usize,
}

impl Proc {
    pub const fn new(id: usize, int: [Interrupt; 256]) -> Self {
        Proc { id }
    }
}

// TODO: Move to a test with recursive repeat groups
const PROCS: [Proc; 256] = {
    seq!(N in 0..256 {
        [
            #(
                Proc::new(
                    N,
                    {
                        [
                        #(
                            Interrupt::Irq~N,
                        )*
                        ]
                    }
                ),
            )*
        ]
    })
};

fn main() {}
