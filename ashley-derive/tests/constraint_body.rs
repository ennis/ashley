use ashley_derive::operation_constraint_match_body;

#[test]
fn test_op_constraint_match_body() {
    operation_constraint_match_body!(
        AddOp { lhs, rhs, op }

        op: "base.add"[wrap:WrapMode] (lhs: ty, rhs: ty)

        where
            %lhs : ArithmeticValue,
            %rhs : BinaryOp(!ty:ArithmeticType, ArithmeticType),
            %a : Constant
    )
}
