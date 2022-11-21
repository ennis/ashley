
struct Operation {
    name: &'static str,
    attributes: &'static [Attribute],
    operands: &'static [Operand],
    regions: &'static [Region],
}

struct Attribute {
    ty: &'static str,
}

struct Operand {
    name: &'static str,
    ty: &'static str,
}


struct Dialect {
    operations: &'static [Operation],
}

const BASE_DIALECT: Dialect = Dialect {
    operations: &[
        Operation {
            name: "func",
            attributes: &[Attribute"TypeAttr"],
            operands: &[],
            regions: &[]
        }
    ]
};

