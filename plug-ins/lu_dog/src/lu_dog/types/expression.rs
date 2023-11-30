// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"expression-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::a_wait::AWait;
use crate::lu_dog::types::argument::Argument;
use crate::lu_dog::types::block::Block;
use crate::lu_dog::types::call::Call;
use crate::lu_dog::types::debugger::DEBUGGER;
use crate::lu_dog::types::empty_expression::EMPTY_EXPRESSION;
use crate::lu_dog::types::expression_statement::ExpressionStatement;
use crate::lu_dog::types::field_access::FieldAccess;
use crate::lu_dog::types::field_expression::FieldExpression;
use crate::lu_dog::types::for_loop::ForLoop;
use crate::lu_dog::types::grouped::Grouped;
use crate::lu_dog::types::index::Index;
use crate::lu_dog::types::lambda::Lambda;
use crate::lu_dog::types::let_statement::LetStatement;
use crate::lu_dog::types::list_element::ListElement;
use crate::lu_dog::types::list_expression::ListExpression;
use crate::lu_dog::types::literal::Literal;
use crate::lu_dog::types::operator::Operator;
use crate::lu_dog::types::pattern::Pattern;
use crate::lu_dog::types::range_expression::RangeExpression;
use crate::lu_dog::types::result_statement::ResultStatement;
use crate::lu_dog::types::struct_expression::StructExpression;
use crate::lu_dog::types::type_cast::TypeCast;
use crate::lu_dog::types::variable_expression::VariableExpression;
use crate::lu_dog::types::x_if::XIf;
use crate::lu_dog::types::x_match::XMatch;
use crate::lu_dog::types::x_path::XPath;
use crate::lu_dog::types::x_print::XPrint;
use crate::lu_dog::types::x_return::XReturn;
use crate::lu_dog::types::x_value::XValue;
use crate::lu_dog::types::x_value::XValueEnum;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-enum-documentation"}}}
/// An Expression
///
/// Expressions are calculations that render values.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Expression {
    AWait(Uuid),
    Block(Uuid),
    Call(Uuid),
    Debugger(Uuid),
    EmptyExpression(Uuid),
    FieldAccess(Uuid),
    FieldExpression(Uuid),
    ForLoop(Uuid),
    Grouped(Uuid),
    XIf(Uuid),
    Index(Uuid),
    Lambda(Uuid),
    ListElement(Uuid),
    ListExpression(Uuid),
    Literal(Uuid),
    XMatch(Uuid),
    Operator(Uuid),
    XPath(Uuid),
    XPrint(Uuid),
    RangeExpression(Uuid),
    XReturn(Uuid),
    StructExpression(Uuid),
    TypeCast(Uuid),
    VariableExpression(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-implementation"}}}
impl Expression {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-new-impl"}}}
    /// Create a new instance of Expression::AWait
    pub fn new_a_wait(a_wait: &Arc<RwLock<AWait>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = a_wait.read().unwrap().id;
        if let Some(a_wait) = store.exhume_expression(&id) {
            a_wait
        } else {
            let new = Arc::new(RwLock::new(Self::AWait(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Block
    pub fn new_block(block: &Arc<RwLock<Block>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = block.read().unwrap().id;
        if let Some(block) = store.exhume_expression(&id) {
            block
        } else {
            let new = Arc::new(RwLock::new(Self::Block(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Call
    pub fn new_call(call: &Arc<RwLock<Call>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = call.read().unwrap().id;
        if let Some(call) = store.exhume_expression(&id) {
            call
        } else {
            let new = Arc::new(RwLock::new(Self::Call(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Debugger
    pub fn new_debugger(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_expression(&DEBUGGER).unwrap()
    }

    /// Create a new instance of Expression::EmptyExpression
    pub fn new_empty_expression(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_expression(&EMPTY_EXPRESSION).unwrap()
    }

    /// Create a new instance of Expression::FieldAccess
    pub fn new_field_access(
        field_access: &Arc<RwLock<FieldAccess>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = field_access.read().unwrap().id;
        if let Some(field_access) = store.exhume_expression(&id) {
            field_access
        } else {
            let new = Arc::new(RwLock::new(Self::FieldAccess(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::FieldExpression
    pub fn new_field_expression(
        field_expression: &Arc<RwLock<FieldExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = field_expression.read().unwrap().id;
        if let Some(field_expression) = store.exhume_expression(&id) {
            field_expression
        } else {
            let new = Arc::new(RwLock::new(Self::FieldExpression(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::ForLoop
    pub fn new_for_loop(
        for_loop: &Arc<RwLock<ForLoop>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = for_loop.read().unwrap().id;
        if let Some(for_loop) = store.exhume_expression(&id) {
            for_loop
        } else {
            let new = Arc::new(RwLock::new(Self::ForLoop(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Grouped
    pub fn new_grouped(
        grouped: &Arc<RwLock<Grouped>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = grouped.read().unwrap().id;
        if let Some(grouped) = store.exhume_expression(&id) {
            grouped
        } else {
            let new = Arc::new(RwLock::new(Self::Grouped(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::XIf
    pub fn new_x_if(x_if: &Arc<RwLock<XIf>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = x_if.read().unwrap().id;
        if let Some(x_if) = store.exhume_expression(&id) {
            x_if
        } else {
            let new = Arc::new(RwLock::new(Self::XIf(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Index
    pub fn new_index(index: &Arc<RwLock<Index>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = index.read().unwrap().id;
        if let Some(index) = store.exhume_expression(&id) {
            index
        } else {
            let new = Arc::new(RwLock::new(Self::Index(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Lambda
    pub fn new_lambda(lambda: &Arc<RwLock<Lambda>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = lambda.read().unwrap().id;
        if let Some(lambda) = store.exhume_expression(&id) {
            lambda
        } else {
            let new = Arc::new(RwLock::new(Self::Lambda(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::ListElement
    pub fn new_list_element(
        list_element: &Arc<RwLock<ListElement>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = list_element.read().unwrap().id;
        if let Some(list_element) = store.exhume_expression(&id) {
            list_element
        } else {
            let new = Arc::new(RwLock::new(Self::ListElement(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::ListExpression
    pub fn new_list_expression(
        list_expression: &Arc<RwLock<ListExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = list_expression.read().unwrap().id;
        if let Some(list_expression) = store.exhume_expression(&id) {
            list_expression
        } else {
            let new = Arc::new(RwLock::new(Self::ListExpression(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Literal
    pub fn new_literal(
        literal: &Arc<RwLock<Literal>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = literal.read().unwrap().id();
        if let Some(literal) = store.exhume_expression(&id) {
            literal
        } else {
            let new = Arc::new(RwLock::new(Self::Literal(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::XMatch
    pub fn new_x_match(x_match: &Arc<RwLock<XMatch>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = x_match.read().unwrap().id;
        if let Some(x_match) = store.exhume_expression(&id) {
            x_match
        } else {
            let new = Arc::new(RwLock::new(Self::XMatch(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::Operator
    pub fn new_operator(
        operator: &Arc<RwLock<Operator>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = operator.read().unwrap().id;
        if let Some(operator) = store.exhume_expression(&id) {
            operator
        } else {
            let new = Arc::new(RwLock::new(Self::Operator(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::XPath
    pub fn new_x_path(x_path: &Arc<RwLock<XPath>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = x_path.read().unwrap().id;
        if let Some(x_path) = store.exhume_expression(&id) {
            x_path
        } else {
            let new = Arc::new(RwLock::new(Self::XPath(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::XPrint
    pub fn new_x_print(x_print: &Arc<RwLock<XPrint>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = x_print.read().unwrap().id;
        if let Some(x_print) = store.exhume_expression(&id) {
            x_print
        } else {
            let new = Arc::new(RwLock::new(Self::XPrint(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::RangeExpression
    pub fn new_range_expression(
        range_expression: &Arc<RwLock<RangeExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = range_expression.read().unwrap().id;
        if let Some(range_expression) = store.exhume_expression(&id) {
            range_expression
        } else {
            let new = Arc::new(RwLock::new(Self::RangeExpression(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::XReturn
    pub fn new_x_return(
        x_return: &Arc<RwLock<XReturn>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = x_return.read().unwrap().id;
        if let Some(x_return) = store.exhume_expression(&id) {
            x_return
        } else {
            let new = Arc::new(RwLock::new(Self::XReturn(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::StructExpression
    pub fn new_struct_expression(
        struct_expression: &Arc<RwLock<StructExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = struct_expression.read().unwrap().id;
        if let Some(struct_expression) = store.exhume_expression(&id) {
            struct_expression
        } else {
            let new = Arc::new(RwLock::new(Self::StructExpression(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::TypeCast
    pub fn new_type_cast(
        type_cast: &Arc<RwLock<TypeCast>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = type_cast.read().unwrap().id;
        if let Some(type_cast) = store.exhume_expression(&id) {
            type_cast
        } else {
            let new = Arc::new(RwLock::new(Self::TypeCast(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Expression::VariableExpression
    pub fn new_variable_expression(
        variable_expression: &Arc<RwLock<VariableExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = variable_expression.read().unwrap().id;
        if let Some(variable_expression) = store.exhume_expression(&id) {
            variable_expression
        } else {
            let new = Arc::new(RwLock::new(Self::VariableExpression(id)));
            store.inter_expression(new.clone());
            new
        }
    } // wtf?

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::AWait(id) => *id,
            Self::Block(id) => *id,
            Self::Call(id) => *id,
            Self::Debugger(id) => *id,
            Self::EmptyExpression(id) => *id,
            Self::FieldAccess(id) => *id,
            Self::FieldExpression(id) => *id,
            Self::ForLoop(id) => *id,
            Self::Grouped(id) => *id,
            Self::XIf(id) => *id,
            Self::Index(id) => *id,
            Self::Lambda(id) => *id,
            Self::ListElement(id) => *id,
            Self::ListExpression(id) => *id,
            Self::Literal(id) => *id,
            Self::XMatch(id) => *id,
            Self::Operator(id) => *id,
            Self::XPath(id) => *id,
            Self::XPrint(id) => *id,
            Self::RangeExpression(id) => *id,
            Self::XReturn(id) => *id,
            Self::StructExpression(id) => *id,
            Self::TypeCast(id) => *id,
            Self::VariableExpression(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-argument"}}}
    /// Navigate to [`Argument`] across R37(1-M)
    pub fn r37_argument<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Argument>>> {
        store
            .iter_argument()
            .filter(|argument| argument.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-cond-to-a_wait"}}}
    /// Navigate to [`AWait`] across R98(1-1c)
    pub fn r98c_a_wait<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<AWait>>> {
        let a_wait = store
            .iter_a_wait()
            .find(|a_wait| a_wait.read().unwrap().x_future == self.id());
        match a_wait {
            Some(ref a_wait) => vec![a_wait.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_Mc-to-call"}}}
    /// Navigate to [`Call`] across R29(1-Mc)
    pub fn r29_call<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Call>>> {
        store
            .iter_call()
            .filter(|call| call.read().unwrap().expression == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-expression_statement"}}}
    /// Navigate to [`ExpressionStatement`] across R31(1-M)
    pub fn r31_expression_statement<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<ExpressionStatement>>> {
        store
            .iter_expression_statement()
            .filter(|expression_statement| {
                expression_statement.read().unwrap().expression == self.id()
            })
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-field_access"}}}
    /// Navigate to [`FieldAccess`] across R27(1-M)
    pub fn r27_field_access<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<FieldAccess>>> {
        store
            .iter_field_access()
            .filter(|field_access| field_access.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-field_expression"}}}
    /// Navigate to [`FieldExpression`] across R38(1-M)
    pub fn r38_field_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<FieldExpression>>> {
        store
            .iter_field_expression()
            .filter(|field_expression| field_expression.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-for_loop"}}}
    /// Navigate to [`ForLoop`] across R42(1-M)
    pub fn r42_for_loop<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ForLoop>>> {
        store
            .iter_for_loop()
            .filter(|for_loop| for_loop.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-grouped"}}}
    /// Navigate to [`Grouped`] across R61(1-M)
    pub fn r61_grouped<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Grouped>>> {
        store
            .iter_grouped()
            .filter(|grouped| grouped.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-x_if"}}}
    /// Navigate to [`XIf`] across R44(1-M)
    pub fn r44_x_if<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XIf>>> {
        store
            .iter_x_if()
            .filter(|x_if| x_if.read().unwrap().test == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-index"}}}
    /// Navigate to [`Index`] across R56(1-M)
    pub fn r56_index<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Index>>> {
        store
            .iter_index()
            .filter(|index| index.read().unwrap().index == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-index"}}}
    /// Navigate to [`Index`] across R57(1-M)
    pub fn r57_index<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Index>>> {
        store
            .iter_index()
            .filter(|index| index.read().unwrap().target == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-cond-to-let_statement"}}}
    /// Navigate to [`LetStatement`] across R20(1-1c)
    pub fn r20c_let_statement<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<LetStatement>>> {
        let let_statement = store
            .iter_let_statement()
            .find(|let_statement| let_statement.read().unwrap().expression == self.id());
        match let_statement {
            Some(ref let_statement) => vec![let_statement.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-list_element"}}}
    /// Navigate to [`ListElement`] across R55(1-M)
    pub fn r55_list_element<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ListElement>>> {
        store
            .iter_list_element()
            .filter(|list_element| list_element.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-x_match"}}}
    /// Navigate to [`XMatch`] across R91(1-M)
    pub fn r91_x_match<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XMatch>>> {
        store
            .iter_x_match()
            .filter(|x_match| x_match.read().unwrap().scrutinee == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_Mc-to-operator"}}}
    /// Navigate to [`Operator`] across R51(1-Mc)
    pub fn r51_operator<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Operator>>> {
        store
            .iter_operator()
            .filter(|operator| operator.read().unwrap().rhs == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-operator"}}}
    /// Navigate to [`Operator`] across R50(1-M)
    pub fn r50_operator<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Operator>>> {
        store
            .iter_operator()
            .filter(|operator| operator.read().unwrap().lhs == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-pattern"}}}
    /// Navigate to [`Pattern`] across R92(1-M)
    pub fn r92_pattern<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Pattern>>> {
        store
            .iter_pattern()
            .filter(|pattern| pattern.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-x_print"}}}
    /// Navigate to [`XPrint`] across R32(1-M)
    pub fn r32_x_print<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XPrint>>> {
        store
            .iter_x_print()
            .filter(|x_print| x_print.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_Mc-to-range_expression"}}}
    /// Navigate to [`RangeExpression`] across R58(1-Mc)
    pub fn r58_range_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<RangeExpression>>> {
        store
            .iter_range_expression()
            .filter(|range_expression| range_expression.read().unwrap().lhs == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_Mc-to-range_expression"}}}
    /// Navigate to [`RangeExpression`] across R59(1-Mc)
    pub fn r59_range_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<RangeExpression>>> {
        store
            .iter_range_expression()
            .filter(|range_expression| range_expression.read().unwrap().rhs == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-result_statement"}}}
    /// Navigate to [`ResultStatement`] across R41(1-M)
    pub fn r41_result_statement<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<ResultStatement>>> {
        store
            .iter_result_statement()
            .filter(|result_statement| result_statement.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-x_return"}}}
    /// Navigate to [`XReturn`] across R45(1-M)
    pub fn r45_x_return<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XReturn>>> {
        store
            .iter_x_return()
            .filter(|x_return| x_return.read().unwrap().expression == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-1_M-to-type_cast"}}}
    /// Navigate to [`TypeCast`] across R68(1-M)
    pub fn r68_type_cast<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<TypeCast>>> {
        store
            .iter_type_cast()
            .filter(|type_cast| type_cast.read().unwrap().lhs == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-struct-impl-nav-backward-assoc-many-to-pattern"}}}
    /// Navigate to [`Pattern`] across R87(1-M)
    pub fn r87_pattern<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Pattern>>> {
        store
            .iter_pattern()
            .filter(|pattern| pattern.read().unwrap().match_expr == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression-impl-nav-subtype-to-supertype-x_value"}}}
    // Navigate to [`XValue`] across R11(isa)
    pub fn r11_x_value<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XValue>>> {
        vec![store
            .iter_x_value()
            .find(|x_value| {
                if let XValueEnum::Expression(id) = x_value.read().unwrap().subtype {
                    id == self.id()
                } else {
                    false
                }
            })
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
