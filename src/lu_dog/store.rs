//! lu_dog Object Store
//!
//! The ObjectStore contains instances of objects in the domain.
//! The instances are stored in a hash map, keyed by the object's UUID.
//! This is used during code generation, and probably not useful elsewhere.
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"lu_dog-object-store-file"}}}
//!
//! # Contents:
//!
//! * [`Argument`]
//! * [`Binary`]
//! * [`Block`]
//! * [`BooleanLiteral`]
//! * [`Call`]
//! * [`Comparison`]
//! * [`DwarfSourceFile`]
//! * [`Error`]
//! * [`ErrorExpression`]
//! * [`Expression`]
//! * [`ExpressionStatement`]
//! * [`Field`]
//! * [`FieldAccess`]
//! * [`FieldExpression`]
//! * [`FloatLiteral`]
//! * [`ForLoop`]
//! * [`Function`]
//! * [`Grouped`]
//! * [`XIf`]
//! * [`Implementation`]
//! * [`Import`]
//! * [`Index`]
//! * [`IntegerLiteral`]
//! * [`Item`]
//! * [`LetStatement`]
//! * [`List`]
//! * [`ListElement`]
//! * [`ListExpression`]
//! * [`Literal`]
//! * [`LocalVariable`]
//! * [`MethodCall`]
//! * [`ZObjectStore`]
//! * [`Operator`]
//! * [`WoogOption`]
//! * [`Parameter`]
//! * [`Print`]
//! * [`RangeExpression`]
//! * [`Reference`]
//! * [`ResultStatement`]
//! * [`XReturn`]
//! * [`ZSome`]
//! * [`Span`]
//! * [`Statement`]
//! * [`StaticMethodCall`]
//! * [`StringLiteral`]
//! * [`WoogStruct`]
//! * [`StructExpression`]
//! * [`XValue`]
//! * [`ValueType`]
//! * [`Variable`]
//! * [`VariableExpression`]
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"lu_dog-object-store-definition"}}}
use std::sync::{Arc, RwLock};
use std::{
    fs,
    io::{self, prelude::*},
    path::Path,
};

use fnv::FnvHashMap as HashMap;
use heck::{ToLowerCamelCase, ToUpperCamelCase};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::lu_dog::types::{
    Argument, Binary, Block, BooleanLiteral, Call, Comparison, DwarfSourceFile, Error,
    ErrorExpression, Expression, ExpressionStatement, Field, FieldAccess, FieldExpression,
    FloatLiteral, ForLoop, Function, Grouped, Implementation, Import, Index, IntegerLiteral, Item,
    LetStatement, List, ListElement, ListExpression, Literal, LocalVariable, MethodCall, Operator,
    Parameter, Print, RangeExpression, Reference, ResultStatement, Span, Statement,
    StaticMethodCall, StringLiteral, StructExpression, ValueType, Variable, VariableExpression,
    WoogOption, WoogStruct, XIf, XReturn, XValue, ZObjectStore, ZSome, ADDITION, ASSIGNMENT,
    DEBUGGER, DIVISION, EMPTY, EQUAL, FALSE_LITERAL, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL,
    MULTIPLICATION, RANGE, SUBTRACTION, TRUE_LITERAL, UNKNOWN, UNKNOWN_VARIABLE,
};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ObjectStore {
    argument: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Argument>>>>>,
    binary: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Binary>>>>>,
    block: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Block>>>>>,
    boolean_literal: Arc<RwLock<HashMap<Uuid, Arc<RwLock<BooleanLiteral>>>>>,
    call: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Call>>>>>,
    comparison: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Comparison>>>>>,
    dwarf_source_file: Arc<RwLock<HashMap<Uuid, Arc<RwLock<DwarfSourceFile>>>>>,
    error: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Error>>>>>,
    error_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ErrorExpression>>>>>,
    expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Expression>>>>>,
    expression_statement: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ExpressionStatement>>>>>,
    field: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Field>>>>>,
    field_access: Arc<RwLock<HashMap<Uuid, Arc<RwLock<FieldAccess>>>>>,
    field_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<FieldExpression>>>>>,
    float_literal: Arc<RwLock<HashMap<Uuid, Arc<RwLock<FloatLiteral>>>>>,
    for_loop: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ForLoop>>>>>,
    function: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Function>>>>>,
    function_id_by_name: Arc<RwLock<HashMap<String, Uuid>>>,
    grouped: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Grouped>>>>>,
    x_if: Arc<RwLock<HashMap<Uuid, Arc<RwLock<XIf>>>>>,
    implementation: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Implementation>>>>>,
    import: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Import>>>>>,
    index: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Index>>>>>,
    integer_literal: Arc<RwLock<HashMap<Uuid, Arc<RwLock<IntegerLiteral>>>>>,
    item: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Item>>>>>,
    let_statement: Arc<RwLock<HashMap<Uuid, Arc<RwLock<LetStatement>>>>>,
    list: Arc<RwLock<HashMap<Uuid, Arc<RwLock<List>>>>>,
    list_element: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ListElement>>>>>,
    list_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ListExpression>>>>>,
    literal: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Literal>>>>>,
    local_variable: Arc<RwLock<HashMap<Uuid, Arc<RwLock<LocalVariable>>>>>,
    method_call: Arc<RwLock<HashMap<Uuid, Arc<RwLock<MethodCall>>>>>,
    z_object_store: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ZObjectStore>>>>>,
    operator: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Operator>>>>>,
    woog_option: Arc<RwLock<HashMap<Uuid, Arc<RwLock<WoogOption>>>>>,
    parameter: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Parameter>>>>>,
    print: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Print>>>>>,
    range_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<RangeExpression>>>>>,
    reference: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Reference>>>>>,
    result_statement: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ResultStatement>>>>>,
    x_return: Arc<RwLock<HashMap<Uuid, Arc<RwLock<XReturn>>>>>,
    z_some: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ZSome>>>>>,
    span: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Span>>>>>,
    statement: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Statement>>>>>,
    static_method_call: Arc<RwLock<HashMap<Uuid, Arc<RwLock<StaticMethodCall>>>>>,
    string_literal: Arc<RwLock<HashMap<Uuid, Arc<RwLock<StringLiteral>>>>>,
    woog_struct: Arc<RwLock<HashMap<Uuid, Arc<RwLock<WoogStruct>>>>>,
    woog_struct_id_by_name: Arc<RwLock<HashMap<String, Uuid>>>,
    struct_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<StructExpression>>>>>,
    x_value: Arc<RwLock<HashMap<Uuid, Arc<RwLock<XValue>>>>>,
    value_type: Arc<RwLock<HashMap<Uuid, Arc<RwLock<ValueType>>>>>,
    variable: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Variable>>>>>,
    variable_expression: Arc<RwLock<HashMap<Uuid, Arc<RwLock<VariableExpression>>>>>,
}

impl ObjectStore {
    pub fn new() -> Self {
        let mut store = Self {
            argument: Arc::new(RwLock::new(HashMap::default())),
            binary: Arc::new(RwLock::new(HashMap::default())),
            block: Arc::new(RwLock::new(HashMap::default())),
            boolean_literal: Arc::new(RwLock::new(HashMap::default())),
            call: Arc::new(RwLock::new(HashMap::default())),
            comparison: Arc::new(RwLock::new(HashMap::default())),
            dwarf_source_file: Arc::new(RwLock::new(HashMap::default())),
            error: Arc::new(RwLock::new(HashMap::default())),
            error_expression: Arc::new(RwLock::new(HashMap::default())),
            expression: Arc::new(RwLock::new(HashMap::default())),
            expression_statement: Arc::new(RwLock::new(HashMap::default())),
            field: Arc::new(RwLock::new(HashMap::default())),
            field_access: Arc::new(RwLock::new(HashMap::default())),
            field_expression: Arc::new(RwLock::new(HashMap::default())),
            float_literal: Arc::new(RwLock::new(HashMap::default())),
            for_loop: Arc::new(RwLock::new(HashMap::default())),
            function: Arc::new(RwLock::new(HashMap::default())),
            function_id_by_name: Arc::new(RwLock::new(HashMap::default())),
            grouped: Arc::new(RwLock::new(HashMap::default())),
            x_if: Arc::new(RwLock::new(HashMap::default())),
            implementation: Arc::new(RwLock::new(HashMap::default())),
            import: Arc::new(RwLock::new(HashMap::default())),
            index: Arc::new(RwLock::new(HashMap::default())),
            integer_literal: Arc::new(RwLock::new(HashMap::default())),
            item: Arc::new(RwLock::new(HashMap::default())),
            let_statement: Arc::new(RwLock::new(HashMap::default())),
            list: Arc::new(RwLock::new(HashMap::default())),
            list_element: Arc::new(RwLock::new(HashMap::default())),
            list_expression: Arc::new(RwLock::new(HashMap::default())),
            literal: Arc::new(RwLock::new(HashMap::default())),
            local_variable: Arc::new(RwLock::new(HashMap::default())),
            method_call: Arc::new(RwLock::new(HashMap::default())),
            z_object_store: Arc::new(RwLock::new(HashMap::default())),
            operator: Arc::new(RwLock::new(HashMap::default())),
            woog_option: Arc::new(RwLock::new(HashMap::default())),
            parameter: Arc::new(RwLock::new(HashMap::default())),
            print: Arc::new(RwLock::new(HashMap::default())),
            range_expression: Arc::new(RwLock::new(HashMap::default())),
            reference: Arc::new(RwLock::new(HashMap::default())),
            result_statement: Arc::new(RwLock::new(HashMap::default())),
            x_return: Arc::new(RwLock::new(HashMap::default())),
            z_some: Arc::new(RwLock::new(HashMap::default())),
            span: Arc::new(RwLock::new(HashMap::default())),
            statement: Arc::new(RwLock::new(HashMap::default())),
            static_method_call: Arc::new(RwLock::new(HashMap::default())),
            string_literal: Arc::new(RwLock::new(HashMap::default())),
            woog_struct: Arc::new(RwLock::new(HashMap::default())),
            woog_struct_id_by_name: Arc::new(RwLock::new(HashMap::default())),
            struct_expression: Arc::new(RwLock::new(HashMap::default())),
            x_value: Arc::new(RwLock::new(HashMap::default())),
            value_type: Arc::new(RwLock::new(HashMap::default())),
            variable: Arc::new(RwLock::new(HashMap::default())),
            variable_expression: Arc::new(RwLock::new(HashMap::default())),
        };

        // Initialize Singleton Subtypes
        // 💥 Look at how beautiful this generated code is for super/sub-type graphs!
        // I remember having a bit of a struggle making it work. It's recursive, with
        // a lot of special cases, and I think it calls other recursive functions...💥
        store.inter_binary(Arc::new(RwLock::new(Binary::Addition(ADDITION))));
        store.inter_binary(Arc::new(RwLock::new(Binary::Assignment(ASSIGNMENT))));
        store.inter_binary(Arc::new(RwLock::new(Binary::Division(DIVISION))));
        store.inter_binary(Arc::new(RwLock::new(Binary::Multiplication(
            MULTIPLICATION,
        ))));
        store.inter_binary(Arc::new(RwLock::new(Binary::Subtraction(SUBTRACTION))));
        store.inter_boolean_literal(Arc::new(RwLock::new(BooleanLiteral::FalseLiteral(
            FALSE_LITERAL,
        ))));
        store.inter_boolean_literal(Arc::new(RwLock::new(BooleanLiteral::TrueLiteral(
            TRUE_LITERAL,
        ))));
        store.inter_comparison(Arc::new(RwLock::new(Comparison::Equal(EQUAL))));
        store.inter_comparison(Arc::new(RwLock::new(Comparison::GreaterThanOrEqual(
            GREATER_THAN_OR_EQUAL,
        ))));
        store.inter_comparison(Arc::new(RwLock::new(Comparison::LessThanOrEqual(
            LESS_THAN_OR_EQUAL,
        ))));
        store.inter_error(Arc::new(RwLock::new(Error::UnknownVariable(
            UNKNOWN_VARIABLE,
        ))));
        store.inter_expression(Arc::new(RwLock::new(Expression::Debugger(DEBUGGER))));
        store.inter_expression(Arc::new(RwLock::new(Expression::Literal(
            Literal::BooleanLiteral(BooleanLiteral::FalseLiteral(FALSE_LITERAL).id()).id(),
        ))));
        store.inter_expression(Arc::new(RwLock::new(Expression::Literal(
            Literal::BooleanLiteral(BooleanLiteral::TrueLiteral(TRUE_LITERAL).id()).id(),
        ))));
        store.inter_literal(Arc::new(RwLock::new(Literal::BooleanLiteral(
            BooleanLiteral::FalseLiteral(FALSE_LITERAL).id(),
        ))));
        store.inter_literal(Arc::new(RwLock::new(Literal::BooleanLiteral(
            BooleanLiteral::TrueLiteral(TRUE_LITERAL).id(),
        ))));
        store.inter_value_type(Arc::new(RwLock::new(ValueType::Empty(EMPTY))));
        store.inter_value_type(Arc::new(RwLock::new(ValueType::Error(
            Error::UnknownVariable(UNKNOWN_VARIABLE).id(),
        ))));
        store.inter_value_type(Arc::new(RwLock::new(ValueType::Range(RANGE))));
        store.inter_value_type(Arc::new(RwLock::new(ValueType::Unknown(UNKNOWN))));

        store
    }

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"lu_dog-object-store-methods"}}}
    /// Inter (insert) [`Argument`] into the store.
    ///
    pub fn inter_argument(&mut self, argument: Arc<RwLock<Argument>>) {
        let read = argument.read().unwrap();
        self.argument
            .write()
            .unwrap()
            .insert(read.id, argument.clone());
    }

    /// Exhume (get) [`Argument`] from the store.
    ///
    pub fn exhume_argument(&self, id: &Uuid) -> Option<Arc<RwLock<Argument>>> {
        self.argument
            .read()
            .unwrap()
            .get(id)
            .map(|argument| argument.clone())
    }

    /// Exorcise (remove) [`Argument`] from the store.
    ///
    pub fn exorcise_argument(&mut self, id: &Uuid) -> Option<Arc<RwLock<Argument>>> {
        self.argument
            .write()
            .unwrap()
            .remove(id)
            .map(|argument| argument.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Argument>`.
    ///
    pub fn iter_argument(&self) -> impl Iterator<Item = Arc<RwLock<Argument>>> + '_ {
        let values: Vec<Arc<RwLock<Argument>>> = self
            .argument
            .read()
            .unwrap()
            .values()
            .map(|argument| argument.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Binary`] into the store.
    ///
    pub fn inter_binary(&mut self, binary: Arc<RwLock<Binary>>) {
        let read = binary.read().unwrap();
        self.binary
            .write()
            .unwrap()
            .insert(read.id(), binary.clone());
    }

    /// Exhume (get) [`Binary`] from the store.
    ///
    pub fn exhume_binary(&self, id: &Uuid) -> Option<Arc<RwLock<Binary>>> {
        self.binary
            .read()
            .unwrap()
            .get(id)
            .map(|binary| binary.clone())
    }

    /// Exorcise (remove) [`Binary`] from the store.
    ///
    pub fn exorcise_binary(&mut self, id: &Uuid) -> Option<Arc<RwLock<Binary>>> {
        self.binary
            .write()
            .unwrap()
            .remove(id)
            .map(|binary| binary.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Binary>`.
    ///
    pub fn iter_binary(&self) -> impl Iterator<Item = Arc<RwLock<Binary>>> + '_ {
        let values: Vec<Arc<RwLock<Binary>>> = self
            .binary
            .read()
            .unwrap()
            .values()
            .map(|binary| binary.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Block`] into the store.
    ///
    pub fn inter_block(&mut self, block: Arc<RwLock<Block>>) {
        let read = block.read().unwrap();
        self.block.write().unwrap().insert(read.id, block.clone());
    }

    /// Exhume (get) [`Block`] from the store.
    ///
    pub fn exhume_block(&self, id: &Uuid) -> Option<Arc<RwLock<Block>>> {
        self.block
            .read()
            .unwrap()
            .get(id)
            .map(|block| block.clone())
    }

    /// Exorcise (remove) [`Block`] from the store.
    ///
    pub fn exorcise_block(&mut self, id: &Uuid) -> Option<Arc<RwLock<Block>>> {
        self.block
            .write()
            .unwrap()
            .remove(id)
            .map(|block| block.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Block>`.
    ///
    pub fn iter_block(&self) -> impl Iterator<Item = Arc<RwLock<Block>>> + '_ {
        let values: Vec<Arc<RwLock<Block>>> = self
            .block
            .read()
            .unwrap()
            .values()
            .map(|block| block.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`BooleanLiteral`] into the store.
    ///
    pub fn inter_boolean_literal(&mut self, boolean_literal: Arc<RwLock<BooleanLiteral>>) {
        let read = boolean_literal.read().unwrap();
        self.boolean_literal
            .write()
            .unwrap()
            .insert(read.id(), boolean_literal.clone());
    }

    /// Exhume (get) [`BooleanLiteral`] from the store.
    ///
    pub fn exhume_boolean_literal(&self, id: &Uuid) -> Option<Arc<RwLock<BooleanLiteral>>> {
        self.boolean_literal
            .read()
            .unwrap()
            .get(id)
            .map(|boolean_literal| boolean_literal.clone())
    }

    /// Exorcise (remove) [`BooleanLiteral`] from the store.
    ///
    pub fn exorcise_boolean_literal(&mut self, id: &Uuid) -> Option<Arc<RwLock<BooleanLiteral>>> {
        self.boolean_literal
            .write()
            .unwrap()
            .remove(id)
            .map(|boolean_literal| boolean_literal.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, BooleanLiteral>`.
    ///
    pub fn iter_boolean_literal(&self) -> impl Iterator<Item = Arc<RwLock<BooleanLiteral>>> + '_ {
        let values: Vec<Arc<RwLock<BooleanLiteral>>> = self
            .boolean_literal
            .read()
            .unwrap()
            .values()
            .map(|boolean_literal| boolean_literal.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Call`] into the store.
    ///
    pub fn inter_call(&mut self, call: Arc<RwLock<Call>>) {
        let read = call.read().unwrap();
        self.call.write().unwrap().insert(read.id, call.clone());
    }

    /// Exhume (get) [`Call`] from the store.
    ///
    pub fn exhume_call(&self, id: &Uuid) -> Option<Arc<RwLock<Call>>> {
        self.call.read().unwrap().get(id).map(|call| call.clone())
    }

    /// Exorcise (remove) [`Call`] from the store.
    ///
    pub fn exorcise_call(&mut self, id: &Uuid) -> Option<Arc<RwLock<Call>>> {
        self.call
            .write()
            .unwrap()
            .remove(id)
            .map(|call| call.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Call>`.
    ///
    pub fn iter_call(&self) -> impl Iterator<Item = Arc<RwLock<Call>>> + '_ {
        let values: Vec<Arc<RwLock<Call>>> = self
            .call
            .read()
            .unwrap()
            .values()
            .map(|call| call.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Comparison`] into the store.
    ///
    pub fn inter_comparison(&mut self, comparison: Arc<RwLock<Comparison>>) {
        let read = comparison.read().unwrap();
        self.comparison
            .write()
            .unwrap()
            .insert(read.id(), comparison.clone());
    }

    /// Exhume (get) [`Comparison`] from the store.
    ///
    pub fn exhume_comparison(&self, id: &Uuid) -> Option<Arc<RwLock<Comparison>>> {
        self.comparison
            .read()
            .unwrap()
            .get(id)
            .map(|comparison| comparison.clone())
    }

    /// Exorcise (remove) [`Comparison`] from the store.
    ///
    pub fn exorcise_comparison(&mut self, id: &Uuid) -> Option<Arc<RwLock<Comparison>>> {
        self.comparison
            .write()
            .unwrap()
            .remove(id)
            .map(|comparison| comparison.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Comparison>`.
    ///
    pub fn iter_comparison(&self) -> impl Iterator<Item = Arc<RwLock<Comparison>>> + '_ {
        let values: Vec<Arc<RwLock<Comparison>>> = self
            .comparison
            .read()
            .unwrap()
            .values()
            .map(|comparison| comparison.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`DwarfSourceFile`] into the store.
    ///
    pub fn inter_dwarf_source_file(&mut self, dwarf_source_file: Arc<RwLock<DwarfSourceFile>>) {
        let read = dwarf_source_file.read().unwrap();
        self.dwarf_source_file
            .write()
            .unwrap()
            .insert(read.id, dwarf_source_file.clone());
    }

    /// Exhume (get) [`DwarfSourceFile`] from the store.
    ///
    pub fn exhume_dwarf_source_file(&self, id: &Uuid) -> Option<Arc<RwLock<DwarfSourceFile>>> {
        self.dwarf_source_file
            .read()
            .unwrap()
            .get(id)
            .map(|dwarf_source_file| dwarf_source_file.clone())
    }

    /// Exorcise (remove) [`DwarfSourceFile`] from the store.
    ///
    pub fn exorcise_dwarf_source_file(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<DwarfSourceFile>>> {
        self.dwarf_source_file
            .write()
            .unwrap()
            .remove(id)
            .map(|dwarf_source_file| dwarf_source_file.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, DwarfSourceFile>`.
    ///
    pub fn iter_dwarf_source_file(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<DwarfSourceFile>>> + '_ {
        let values: Vec<Arc<RwLock<DwarfSourceFile>>> = self
            .dwarf_source_file
            .read()
            .unwrap()
            .values()
            .map(|dwarf_source_file| dwarf_source_file.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Error`] into the store.
    ///
    pub fn inter_error(&mut self, error: Arc<RwLock<Error>>) {
        let read = error.read().unwrap();
        self.error.write().unwrap().insert(read.id(), error.clone());
    }

    /// Exhume (get) [`Error`] from the store.
    ///
    pub fn exhume_error(&self, id: &Uuid) -> Option<Arc<RwLock<Error>>> {
        self.error
            .read()
            .unwrap()
            .get(id)
            .map(|error| error.clone())
    }

    /// Exorcise (remove) [`Error`] from the store.
    ///
    pub fn exorcise_error(&mut self, id: &Uuid) -> Option<Arc<RwLock<Error>>> {
        self.error
            .write()
            .unwrap()
            .remove(id)
            .map(|error| error.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Error>`.
    ///
    pub fn iter_error(&self) -> impl Iterator<Item = Arc<RwLock<Error>>> + '_ {
        let values: Vec<Arc<RwLock<Error>>> = self
            .error
            .read()
            .unwrap()
            .values()
            .map(|error| error.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ErrorExpression`] into the store.
    ///
    pub fn inter_error_expression(&mut self, error_expression: Arc<RwLock<ErrorExpression>>) {
        let read = error_expression.read().unwrap();
        self.error_expression
            .write()
            .unwrap()
            .insert(read.id, error_expression.clone());
    }

    /// Exhume (get) [`ErrorExpression`] from the store.
    ///
    pub fn exhume_error_expression(&self, id: &Uuid) -> Option<Arc<RwLock<ErrorExpression>>> {
        self.error_expression
            .read()
            .unwrap()
            .get(id)
            .map(|error_expression| error_expression.clone())
    }

    /// Exorcise (remove) [`ErrorExpression`] from the store.
    ///
    pub fn exorcise_error_expression(&mut self, id: &Uuid) -> Option<Arc<RwLock<ErrorExpression>>> {
        self.error_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|error_expression| error_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ErrorExpression>`.
    ///
    pub fn iter_error_expression(&self) -> impl Iterator<Item = Arc<RwLock<ErrorExpression>>> + '_ {
        let values: Vec<Arc<RwLock<ErrorExpression>>> = self
            .error_expression
            .read()
            .unwrap()
            .values()
            .map(|error_expression| error_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Expression`] into the store.
    ///
    pub fn inter_expression(&mut self, expression: Arc<RwLock<Expression>>) {
        let read = expression.read().unwrap();
        self.expression
            .write()
            .unwrap()
            .insert(read.id(), expression.clone());
    }

    /// Exhume (get) [`Expression`] from the store.
    ///
    pub fn exhume_expression(&self, id: &Uuid) -> Option<Arc<RwLock<Expression>>> {
        self.expression
            .read()
            .unwrap()
            .get(id)
            .map(|expression| expression.clone())
    }

    /// Exorcise (remove) [`Expression`] from the store.
    ///
    pub fn exorcise_expression(&mut self, id: &Uuid) -> Option<Arc<RwLock<Expression>>> {
        self.expression
            .write()
            .unwrap()
            .remove(id)
            .map(|expression| expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Expression>`.
    ///
    pub fn iter_expression(&self) -> impl Iterator<Item = Arc<RwLock<Expression>>> + '_ {
        let values: Vec<Arc<RwLock<Expression>>> = self
            .expression
            .read()
            .unwrap()
            .values()
            .map(|expression| expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ExpressionStatement`] into the store.
    ///
    pub fn inter_expression_statement(
        &mut self,
        expression_statement: Arc<RwLock<ExpressionStatement>>,
    ) {
        let read = expression_statement.read().unwrap();
        self.expression_statement
            .write()
            .unwrap()
            .insert(read.id, expression_statement.clone());
    }

    /// Exhume (get) [`ExpressionStatement`] from the store.
    ///
    pub fn exhume_expression_statement(
        &self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<ExpressionStatement>>> {
        self.expression_statement
            .read()
            .unwrap()
            .get(id)
            .map(|expression_statement| expression_statement.clone())
    }

    /// Exorcise (remove) [`ExpressionStatement`] from the store.
    ///
    pub fn exorcise_expression_statement(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<ExpressionStatement>>> {
        self.expression_statement
            .write()
            .unwrap()
            .remove(id)
            .map(|expression_statement| expression_statement.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ExpressionStatement>`.
    ///
    pub fn iter_expression_statement(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<ExpressionStatement>>> + '_ {
        let values: Vec<Arc<RwLock<ExpressionStatement>>> = self
            .expression_statement
            .read()
            .unwrap()
            .values()
            .map(|expression_statement| expression_statement.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Field`] into the store.
    ///
    pub fn inter_field(&mut self, field: Arc<RwLock<Field>>) {
        let read = field.read().unwrap();
        self.field.write().unwrap().insert(read.id, field.clone());
    }

    /// Exhume (get) [`Field`] from the store.
    ///
    pub fn exhume_field(&self, id: &Uuid) -> Option<Arc<RwLock<Field>>> {
        self.field
            .read()
            .unwrap()
            .get(id)
            .map(|field| field.clone())
    }

    /// Exorcise (remove) [`Field`] from the store.
    ///
    pub fn exorcise_field(&mut self, id: &Uuid) -> Option<Arc<RwLock<Field>>> {
        self.field
            .write()
            .unwrap()
            .remove(id)
            .map(|field| field.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Field>`.
    ///
    pub fn iter_field(&self) -> impl Iterator<Item = Arc<RwLock<Field>>> + '_ {
        let values: Vec<Arc<RwLock<Field>>> = self
            .field
            .read()
            .unwrap()
            .values()
            .map(|field| field.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`FieldAccess`] into the store.
    ///
    pub fn inter_field_access(&mut self, field_access: Arc<RwLock<FieldAccess>>) {
        let read = field_access.read().unwrap();
        self.field_access
            .write()
            .unwrap()
            .insert(read.id, field_access.clone());
    }

    /// Exhume (get) [`FieldAccess`] from the store.
    ///
    pub fn exhume_field_access(&self, id: &Uuid) -> Option<Arc<RwLock<FieldAccess>>> {
        self.field_access
            .read()
            .unwrap()
            .get(id)
            .map(|field_access| field_access.clone())
    }

    /// Exorcise (remove) [`FieldAccess`] from the store.
    ///
    pub fn exorcise_field_access(&mut self, id: &Uuid) -> Option<Arc<RwLock<FieldAccess>>> {
        self.field_access
            .write()
            .unwrap()
            .remove(id)
            .map(|field_access| field_access.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, FieldAccess>`.
    ///
    pub fn iter_field_access(&self) -> impl Iterator<Item = Arc<RwLock<FieldAccess>>> + '_ {
        let values: Vec<Arc<RwLock<FieldAccess>>> = self
            .field_access
            .read()
            .unwrap()
            .values()
            .map(|field_access| field_access.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`FieldExpression`] into the store.
    ///
    pub fn inter_field_expression(&mut self, field_expression: Arc<RwLock<FieldExpression>>) {
        let read = field_expression.read().unwrap();
        self.field_expression
            .write()
            .unwrap()
            .insert(read.id, field_expression.clone());
    }

    /// Exhume (get) [`FieldExpression`] from the store.
    ///
    pub fn exhume_field_expression(&self, id: &Uuid) -> Option<Arc<RwLock<FieldExpression>>> {
        self.field_expression
            .read()
            .unwrap()
            .get(id)
            .map(|field_expression| field_expression.clone())
    }

    /// Exorcise (remove) [`FieldExpression`] from the store.
    ///
    pub fn exorcise_field_expression(&mut self, id: &Uuid) -> Option<Arc<RwLock<FieldExpression>>> {
        self.field_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|field_expression| field_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, FieldExpression>`.
    ///
    pub fn iter_field_expression(&self) -> impl Iterator<Item = Arc<RwLock<FieldExpression>>> + '_ {
        let values: Vec<Arc<RwLock<FieldExpression>>> = self
            .field_expression
            .read()
            .unwrap()
            .values()
            .map(|field_expression| field_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`FloatLiteral`] into the store.
    ///
    pub fn inter_float_literal(&mut self, float_literal: Arc<RwLock<FloatLiteral>>) {
        let read = float_literal.read().unwrap();
        self.float_literal
            .write()
            .unwrap()
            .insert(read.id, float_literal.clone());
    }

    /// Exhume (get) [`FloatLiteral`] from the store.
    ///
    pub fn exhume_float_literal(&self, id: &Uuid) -> Option<Arc<RwLock<FloatLiteral>>> {
        self.float_literal
            .read()
            .unwrap()
            .get(id)
            .map(|float_literal| float_literal.clone())
    }

    /// Exorcise (remove) [`FloatLiteral`] from the store.
    ///
    pub fn exorcise_float_literal(&mut self, id: &Uuid) -> Option<Arc<RwLock<FloatLiteral>>> {
        self.float_literal
            .write()
            .unwrap()
            .remove(id)
            .map(|float_literal| float_literal.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, FloatLiteral>`.
    ///
    pub fn iter_float_literal(&self) -> impl Iterator<Item = Arc<RwLock<FloatLiteral>>> + '_ {
        let values: Vec<Arc<RwLock<FloatLiteral>>> = self
            .float_literal
            .read()
            .unwrap()
            .values()
            .map(|float_literal| float_literal.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ForLoop`] into the store.
    ///
    pub fn inter_for_loop(&mut self, for_loop: Arc<RwLock<ForLoop>>) {
        let read = for_loop.read().unwrap();
        self.for_loop
            .write()
            .unwrap()
            .insert(read.id, for_loop.clone());
    }

    /// Exhume (get) [`ForLoop`] from the store.
    ///
    pub fn exhume_for_loop(&self, id: &Uuid) -> Option<Arc<RwLock<ForLoop>>> {
        self.for_loop
            .read()
            .unwrap()
            .get(id)
            .map(|for_loop| for_loop.clone())
    }

    /// Exorcise (remove) [`ForLoop`] from the store.
    ///
    pub fn exorcise_for_loop(&mut self, id: &Uuid) -> Option<Arc<RwLock<ForLoop>>> {
        self.for_loop
            .write()
            .unwrap()
            .remove(id)
            .map(|for_loop| for_loop.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ForLoop>`.
    ///
    pub fn iter_for_loop(&self) -> impl Iterator<Item = Arc<RwLock<ForLoop>>> + '_ {
        let values: Vec<Arc<RwLock<ForLoop>>> = self
            .for_loop
            .read()
            .unwrap()
            .values()
            .map(|for_loop| for_loop.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Function`] into the store.
    ///
    pub fn inter_function(&mut self, function: Arc<RwLock<Function>>) {
        let read = function.read().unwrap();
        self.function_id_by_name
            .write()
            .unwrap()
            .insert(read.name.to_lower_camel_case(), read.id);
        self.function
            .write()
            .unwrap()
            .insert(read.id, function.clone());
    }

    /// Exhume (get) [`Function`] from the store.
    ///
    pub fn exhume_function(&self, id: &Uuid) -> Option<Arc<RwLock<Function>>> {
        self.function
            .read()
            .unwrap()
            .get(id)
            .map(|function| function.clone())
    }

    pub fn exhume_function_id_by_name(&self, name: &str) -> Option<Uuid> {
        self.function_id_by_name
            .read()
            .unwrap()
            .get(name)
            .map(|id| *id)
    }

    /// Exorcise (remove) [`Function`] from the store.
    ///
    pub fn exorcise_function(&mut self, id: &Uuid) -> Option<Arc<RwLock<Function>>> {
        self.function
            .write()
            .unwrap()
            .remove(id)
            .map(|function| function.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Function>`.
    ///
    pub fn iter_function(&self) -> impl Iterator<Item = Arc<RwLock<Function>>> + '_ {
        let values: Vec<Arc<RwLock<Function>>> = self
            .function
            .read()
            .unwrap()
            .values()
            .map(|function| function.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Grouped`] into the store.
    ///
    pub fn inter_grouped(&mut self, grouped: Arc<RwLock<Grouped>>) {
        let read = grouped.read().unwrap();
        self.grouped
            .write()
            .unwrap()
            .insert(read.id, grouped.clone());
    }

    /// Exhume (get) [`Grouped`] from the store.
    ///
    pub fn exhume_grouped(&self, id: &Uuid) -> Option<Arc<RwLock<Grouped>>> {
        self.grouped
            .read()
            .unwrap()
            .get(id)
            .map(|grouped| grouped.clone())
    }

    /// Exorcise (remove) [`Grouped`] from the store.
    ///
    pub fn exorcise_grouped(&mut self, id: &Uuid) -> Option<Arc<RwLock<Grouped>>> {
        self.grouped
            .write()
            .unwrap()
            .remove(id)
            .map(|grouped| grouped.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Grouped>`.
    ///
    pub fn iter_grouped(&self) -> impl Iterator<Item = Arc<RwLock<Grouped>>> + '_ {
        let values: Vec<Arc<RwLock<Grouped>>> = self
            .grouped
            .read()
            .unwrap()
            .values()
            .map(|grouped| grouped.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`XIf`] into the store.
    ///
    pub fn inter_x_if(&mut self, x_if: Arc<RwLock<XIf>>) {
        let read = x_if.read().unwrap();
        self.x_if.write().unwrap().insert(read.id, x_if.clone());
    }

    /// Exhume (get) [`XIf`] from the store.
    ///
    pub fn exhume_x_if(&self, id: &Uuid) -> Option<Arc<RwLock<XIf>>> {
        self.x_if.read().unwrap().get(id).map(|x_if| x_if.clone())
    }

    /// Exorcise (remove) [`XIf`] from the store.
    ///
    pub fn exorcise_x_if(&mut self, id: &Uuid) -> Option<Arc<RwLock<XIf>>> {
        self.x_if
            .write()
            .unwrap()
            .remove(id)
            .map(|x_if| x_if.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, XIf>`.
    ///
    pub fn iter_x_if(&self) -> impl Iterator<Item = Arc<RwLock<XIf>>> + '_ {
        let values: Vec<Arc<RwLock<XIf>>> = self
            .x_if
            .read()
            .unwrap()
            .values()
            .map(|x_if| x_if.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Implementation`] into the store.
    ///
    pub fn inter_implementation(&mut self, implementation: Arc<RwLock<Implementation>>) {
        let read = implementation.read().unwrap();
        self.implementation
            .write()
            .unwrap()
            .insert(read.id, implementation.clone());
    }

    /// Exhume (get) [`Implementation`] from the store.
    ///
    pub fn exhume_implementation(&self, id: &Uuid) -> Option<Arc<RwLock<Implementation>>> {
        self.implementation
            .read()
            .unwrap()
            .get(id)
            .map(|implementation| implementation.clone())
    }

    /// Exorcise (remove) [`Implementation`] from the store.
    ///
    pub fn exorcise_implementation(&mut self, id: &Uuid) -> Option<Arc<RwLock<Implementation>>> {
        self.implementation
            .write()
            .unwrap()
            .remove(id)
            .map(|implementation| implementation.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Implementation>`.
    ///
    pub fn iter_implementation(&self) -> impl Iterator<Item = Arc<RwLock<Implementation>>> + '_ {
        let values: Vec<Arc<RwLock<Implementation>>> = self
            .implementation
            .read()
            .unwrap()
            .values()
            .map(|implementation| implementation.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Import`] into the store.
    ///
    pub fn inter_import(&mut self, import: Arc<RwLock<Import>>) {
        let read = import.read().unwrap();
        self.import.write().unwrap().insert(read.id, import.clone());
    }

    /// Exhume (get) [`Import`] from the store.
    ///
    pub fn exhume_import(&self, id: &Uuid) -> Option<Arc<RwLock<Import>>> {
        self.import
            .read()
            .unwrap()
            .get(id)
            .map(|import| import.clone())
    }

    /// Exorcise (remove) [`Import`] from the store.
    ///
    pub fn exorcise_import(&mut self, id: &Uuid) -> Option<Arc<RwLock<Import>>> {
        self.import
            .write()
            .unwrap()
            .remove(id)
            .map(|import| import.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Import>`.
    ///
    pub fn iter_import(&self) -> impl Iterator<Item = Arc<RwLock<Import>>> + '_ {
        let values: Vec<Arc<RwLock<Import>>> = self
            .import
            .read()
            .unwrap()
            .values()
            .map(|import| import.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Index`] into the store.
    ///
    pub fn inter_index(&mut self, index: Arc<RwLock<Index>>) {
        let read = index.read().unwrap();
        self.index.write().unwrap().insert(read.id, index.clone());
    }

    /// Exhume (get) [`Index`] from the store.
    ///
    pub fn exhume_index(&self, id: &Uuid) -> Option<Arc<RwLock<Index>>> {
        self.index
            .read()
            .unwrap()
            .get(id)
            .map(|index| index.clone())
    }

    /// Exorcise (remove) [`Index`] from the store.
    ///
    pub fn exorcise_index(&mut self, id: &Uuid) -> Option<Arc<RwLock<Index>>> {
        self.index
            .write()
            .unwrap()
            .remove(id)
            .map(|index| index.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Index>`.
    ///
    pub fn iter_index(&self) -> impl Iterator<Item = Arc<RwLock<Index>>> + '_ {
        let values: Vec<Arc<RwLock<Index>>> = self
            .index
            .read()
            .unwrap()
            .values()
            .map(|index| index.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`IntegerLiteral`] into the store.
    ///
    pub fn inter_integer_literal(&mut self, integer_literal: Arc<RwLock<IntegerLiteral>>) {
        let read = integer_literal.read().unwrap();
        self.integer_literal
            .write()
            .unwrap()
            .insert(read.id, integer_literal.clone());
    }

    /// Exhume (get) [`IntegerLiteral`] from the store.
    ///
    pub fn exhume_integer_literal(&self, id: &Uuid) -> Option<Arc<RwLock<IntegerLiteral>>> {
        self.integer_literal
            .read()
            .unwrap()
            .get(id)
            .map(|integer_literal| integer_literal.clone())
    }

    /// Exorcise (remove) [`IntegerLiteral`] from the store.
    ///
    pub fn exorcise_integer_literal(&mut self, id: &Uuid) -> Option<Arc<RwLock<IntegerLiteral>>> {
        self.integer_literal
            .write()
            .unwrap()
            .remove(id)
            .map(|integer_literal| integer_literal.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, IntegerLiteral>`.
    ///
    pub fn iter_integer_literal(&self) -> impl Iterator<Item = Arc<RwLock<IntegerLiteral>>> + '_ {
        let values: Vec<Arc<RwLock<IntegerLiteral>>> = self
            .integer_literal
            .read()
            .unwrap()
            .values()
            .map(|integer_literal| integer_literal.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Item`] into the store.
    ///
    pub fn inter_item(&mut self, item: Arc<RwLock<Item>>) {
        let read = item.read().unwrap();
        self.item.write().unwrap().insert(read.id, item.clone());
    }

    /// Exhume (get) [`Item`] from the store.
    ///
    pub fn exhume_item(&self, id: &Uuid) -> Option<Arc<RwLock<Item>>> {
        self.item.read().unwrap().get(id).map(|item| item.clone())
    }

    /// Exorcise (remove) [`Item`] from the store.
    ///
    pub fn exorcise_item(&mut self, id: &Uuid) -> Option<Arc<RwLock<Item>>> {
        self.item
            .write()
            .unwrap()
            .remove(id)
            .map(|item| item.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Item>`.
    ///
    pub fn iter_item(&self) -> impl Iterator<Item = Arc<RwLock<Item>>> + '_ {
        let values: Vec<Arc<RwLock<Item>>> = self
            .item
            .read()
            .unwrap()
            .values()
            .map(|item| item.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LetStatement`] into the store.
    ///
    pub fn inter_let_statement(&mut self, let_statement: Arc<RwLock<LetStatement>>) {
        let read = let_statement.read().unwrap();
        self.let_statement
            .write()
            .unwrap()
            .insert(read.id, let_statement.clone());
    }

    /// Exhume (get) [`LetStatement`] from the store.
    ///
    pub fn exhume_let_statement(&self, id: &Uuid) -> Option<Arc<RwLock<LetStatement>>> {
        self.let_statement
            .read()
            .unwrap()
            .get(id)
            .map(|let_statement| let_statement.clone())
    }

    /// Exorcise (remove) [`LetStatement`] from the store.
    ///
    pub fn exorcise_let_statement(&mut self, id: &Uuid) -> Option<Arc<RwLock<LetStatement>>> {
        self.let_statement
            .write()
            .unwrap()
            .remove(id)
            .map(|let_statement| let_statement.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LetStatement>`.
    ///
    pub fn iter_let_statement(&self) -> impl Iterator<Item = Arc<RwLock<LetStatement>>> + '_ {
        let values: Vec<Arc<RwLock<LetStatement>>> = self
            .let_statement
            .read()
            .unwrap()
            .values()
            .map(|let_statement| let_statement.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`List`] into the store.
    ///
    pub fn inter_list(&mut self, list: Arc<RwLock<List>>) {
        let read = list.read().unwrap();
        self.list.write().unwrap().insert(read.id, list.clone());
    }

    /// Exhume (get) [`List`] from the store.
    ///
    pub fn exhume_list(&self, id: &Uuid) -> Option<Arc<RwLock<List>>> {
        self.list.read().unwrap().get(id).map(|list| list.clone())
    }

    /// Exorcise (remove) [`List`] from the store.
    ///
    pub fn exorcise_list(&mut self, id: &Uuid) -> Option<Arc<RwLock<List>>> {
        self.list
            .write()
            .unwrap()
            .remove(id)
            .map(|list| list.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, List>`.
    ///
    pub fn iter_list(&self) -> impl Iterator<Item = Arc<RwLock<List>>> + '_ {
        let values: Vec<Arc<RwLock<List>>> = self
            .list
            .read()
            .unwrap()
            .values()
            .map(|list| list.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ListElement`] into the store.
    ///
    pub fn inter_list_element(&mut self, list_element: Arc<RwLock<ListElement>>) {
        let read = list_element.read().unwrap();
        self.list_element
            .write()
            .unwrap()
            .insert(read.id, list_element.clone());
    }

    /// Exhume (get) [`ListElement`] from the store.
    ///
    pub fn exhume_list_element(&self, id: &Uuid) -> Option<Arc<RwLock<ListElement>>> {
        self.list_element
            .read()
            .unwrap()
            .get(id)
            .map(|list_element| list_element.clone())
    }

    /// Exorcise (remove) [`ListElement`] from the store.
    ///
    pub fn exorcise_list_element(&mut self, id: &Uuid) -> Option<Arc<RwLock<ListElement>>> {
        self.list_element
            .write()
            .unwrap()
            .remove(id)
            .map(|list_element| list_element.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ListElement>`.
    ///
    pub fn iter_list_element(&self) -> impl Iterator<Item = Arc<RwLock<ListElement>>> + '_ {
        let values: Vec<Arc<RwLock<ListElement>>> = self
            .list_element
            .read()
            .unwrap()
            .values()
            .map(|list_element| list_element.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ListExpression`] into the store.
    ///
    pub fn inter_list_expression(&mut self, list_expression: Arc<RwLock<ListExpression>>) {
        let read = list_expression.read().unwrap();
        self.list_expression
            .write()
            .unwrap()
            .insert(read.id, list_expression.clone());
    }

    /// Exhume (get) [`ListExpression`] from the store.
    ///
    pub fn exhume_list_expression(&self, id: &Uuid) -> Option<Arc<RwLock<ListExpression>>> {
        self.list_expression
            .read()
            .unwrap()
            .get(id)
            .map(|list_expression| list_expression.clone())
    }

    /// Exorcise (remove) [`ListExpression`] from the store.
    ///
    pub fn exorcise_list_expression(&mut self, id: &Uuid) -> Option<Arc<RwLock<ListExpression>>> {
        self.list_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|list_expression| list_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ListExpression>`.
    ///
    pub fn iter_list_expression(&self) -> impl Iterator<Item = Arc<RwLock<ListExpression>>> + '_ {
        let values: Vec<Arc<RwLock<ListExpression>>> = self
            .list_expression
            .read()
            .unwrap()
            .values()
            .map(|list_expression| list_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Literal`] into the store.
    ///
    pub fn inter_literal(&mut self, literal: Arc<RwLock<Literal>>) {
        let read = literal.read().unwrap();
        self.literal
            .write()
            .unwrap()
            .insert(read.id(), literal.clone());
    }

    /// Exhume (get) [`Literal`] from the store.
    ///
    pub fn exhume_literal(&self, id: &Uuid) -> Option<Arc<RwLock<Literal>>> {
        self.literal
            .read()
            .unwrap()
            .get(id)
            .map(|literal| literal.clone())
    }

    /// Exorcise (remove) [`Literal`] from the store.
    ///
    pub fn exorcise_literal(&mut self, id: &Uuid) -> Option<Arc<RwLock<Literal>>> {
        self.literal
            .write()
            .unwrap()
            .remove(id)
            .map(|literal| literal.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Literal>`.
    ///
    pub fn iter_literal(&self) -> impl Iterator<Item = Arc<RwLock<Literal>>> + '_ {
        let values: Vec<Arc<RwLock<Literal>>> = self
            .literal
            .read()
            .unwrap()
            .values()
            .map(|literal| literal.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LocalVariable`] into the store.
    ///
    pub fn inter_local_variable(&mut self, local_variable: Arc<RwLock<LocalVariable>>) {
        let read = local_variable.read().unwrap();
        self.local_variable
            .write()
            .unwrap()
            .insert(read.id, local_variable.clone());
    }

    /// Exhume (get) [`LocalVariable`] from the store.
    ///
    pub fn exhume_local_variable(&self, id: &Uuid) -> Option<Arc<RwLock<LocalVariable>>> {
        self.local_variable
            .read()
            .unwrap()
            .get(id)
            .map(|local_variable| local_variable.clone())
    }

    /// Exorcise (remove) [`LocalVariable`] from the store.
    ///
    pub fn exorcise_local_variable(&mut self, id: &Uuid) -> Option<Arc<RwLock<LocalVariable>>> {
        self.local_variable
            .write()
            .unwrap()
            .remove(id)
            .map(|local_variable| local_variable.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LocalVariable>`.
    ///
    pub fn iter_local_variable(&self) -> impl Iterator<Item = Arc<RwLock<LocalVariable>>> + '_ {
        let values: Vec<Arc<RwLock<LocalVariable>>> = self
            .local_variable
            .read()
            .unwrap()
            .values()
            .map(|local_variable| local_variable.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`MethodCall`] into the store.
    ///
    pub fn inter_method_call(&mut self, method_call: Arc<RwLock<MethodCall>>) {
        let read = method_call.read().unwrap();
        self.method_call
            .write()
            .unwrap()
            .insert(read.id, method_call.clone());
    }

    /// Exhume (get) [`MethodCall`] from the store.
    ///
    pub fn exhume_method_call(&self, id: &Uuid) -> Option<Arc<RwLock<MethodCall>>> {
        self.method_call
            .read()
            .unwrap()
            .get(id)
            .map(|method_call| method_call.clone())
    }

    /// Exorcise (remove) [`MethodCall`] from the store.
    ///
    pub fn exorcise_method_call(&mut self, id: &Uuid) -> Option<Arc<RwLock<MethodCall>>> {
        self.method_call
            .write()
            .unwrap()
            .remove(id)
            .map(|method_call| method_call.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, MethodCall>`.
    ///
    pub fn iter_method_call(&self) -> impl Iterator<Item = Arc<RwLock<MethodCall>>> + '_ {
        let values: Vec<Arc<RwLock<MethodCall>>> = self
            .method_call
            .read()
            .unwrap()
            .values()
            .map(|method_call| method_call.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ZObjectStore`] into the store.
    ///
    pub fn inter_z_object_store(&mut self, z_object_store: Arc<RwLock<ZObjectStore>>) {
        let read = z_object_store.read().unwrap();
        self.z_object_store
            .write()
            .unwrap()
            .insert(read.id, z_object_store.clone());
    }

    /// Exhume (get) [`ZObjectStore`] from the store.
    ///
    pub fn exhume_z_object_store(&self, id: &Uuid) -> Option<Arc<RwLock<ZObjectStore>>> {
        self.z_object_store
            .read()
            .unwrap()
            .get(id)
            .map(|z_object_store| z_object_store.clone())
    }

    /// Exorcise (remove) [`ZObjectStore`] from the store.
    ///
    pub fn exorcise_z_object_store(&mut self, id: &Uuid) -> Option<Arc<RwLock<ZObjectStore>>> {
        self.z_object_store
            .write()
            .unwrap()
            .remove(id)
            .map(|z_object_store| z_object_store.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ZObjectStore>`.
    ///
    pub fn iter_z_object_store(&self) -> impl Iterator<Item = Arc<RwLock<ZObjectStore>>> + '_ {
        let values: Vec<Arc<RwLock<ZObjectStore>>> = self
            .z_object_store
            .read()
            .unwrap()
            .values()
            .map(|z_object_store| z_object_store.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Operator`] into the store.
    ///
    pub fn inter_operator(&mut self, operator: Arc<RwLock<Operator>>) {
        let read = operator.read().unwrap();
        self.operator
            .write()
            .unwrap()
            .insert(read.id, operator.clone());
    }

    /// Exhume (get) [`Operator`] from the store.
    ///
    pub fn exhume_operator(&self, id: &Uuid) -> Option<Arc<RwLock<Operator>>> {
        self.operator
            .read()
            .unwrap()
            .get(id)
            .map(|operator| operator.clone())
    }

    /// Exorcise (remove) [`Operator`] from the store.
    ///
    pub fn exorcise_operator(&mut self, id: &Uuid) -> Option<Arc<RwLock<Operator>>> {
        self.operator
            .write()
            .unwrap()
            .remove(id)
            .map(|operator| operator.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Operator>`.
    ///
    pub fn iter_operator(&self) -> impl Iterator<Item = Arc<RwLock<Operator>>> + '_ {
        let values: Vec<Arc<RwLock<Operator>>> = self
            .operator
            .read()
            .unwrap()
            .values()
            .map(|operator| operator.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`WoogOption`] into the store.
    ///
    pub fn inter_woog_option(&mut self, woog_option: Arc<RwLock<WoogOption>>) {
        let read = woog_option.read().unwrap();
        self.woog_option
            .write()
            .unwrap()
            .insert(read.id, woog_option.clone());
    }

    /// Exhume (get) [`WoogOption`] from the store.
    ///
    pub fn exhume_woog_option(&self, id: &Uuid) -> Option<Arc<RwLock<WoogOption>>> {
        self.woog_option
            .read()
            .unwrap()
            .get(id)
            .map(|woog_option| woog_option.clone())
    }

    /// Exorcise (remove) [`WoogOption`] from the store.
    ///
    pub fn exorcise_woog_option(&mut self, id: &Uuid) -> Option<Arc<RwLock<WoogOption>>> {
        self.woog_option
            .write()
            .unwrap()
            .remove(id)
            .map(|woog_option| woog_option.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, WoogOption>`.
    ///
    pub fn iter_woog_option(&self) -> impl Iterator<Item = Arc<RwLock<WoogOption>>> + '_ {
        let values: Vec<Arc<RwLock<WoogOption>>> = self
            .woog_option
            .read()
            .unwrap()
            .values()
            .map(|woog_option| woog_option.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Parameter`] into the store.
    ///
    pub fn inter_parameter(&mut self, parameter: Arc<RwLock<Parameter>>) {
        let read = parameter.read().unwrap();
        self.parameter
            .write()
            .unwrap()
            .insert(read.id, parameter.clone());
    }

    /// Exhume (get) [`Parameter`] from the store.
    ///
    pub fn exhume_parameter(&self, id: &Uuid) -> Option<Arc<RwLock<Parameter>>> {
        self.parameter
            .read()
            .unwrap()
            .get(id)
            .map(|parameter| parameter.clone())
    }

    /// Exorcise (remove) [`Parameter`] from the store.
    ///
    pub fn exorcise_parameter(&mut self, id: &Uuid) -> Option<Arc<RwLock<Parameter>>> {
        self.parameter
            .write()
            .unwrap()
            .remove(id)
            .map(|parameter| parameter.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Parameter>`.
    ///
    pub fn iter_parameter(&self) -> impl Iterator<Item = Arc<RwLock<Parameter>>> + '_ {
        let values: Vec<Arc<RwLock<Parameter>>> = self
            .parameter
            .read()
            .unwrap()
            .values()
            .map(|parameter| parameter.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Print`] into the store.
    ///
    pub fn inter_print(&mut self, print: Arc<RwLock<Print>>) {
        let read = print.read().unwrap();
        self.print.write().unwrap().insert(read.id, print.clone());
    }

    /// Exhume (get) [`Print`] from the store.
    ///
    pub fn exhume_print(&self, id: &Uuid) -> Option<Arc<RwLock<Print>>> {
        self.print
            .read()
            .unwrap()
            .get(id)
            .map(|print| print.clone())
    }

    /// Exorcise (remove) [`Print`] from the store.
    ///
    pub fn exorcise_print(&mut self, id: &Uuid) -> Option<Arc<RwLock<Print>>> {
        self.print
            .write()
            .unwrap()
            .remove(id)
            .map(|print| print.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Print>`.
    ///
    pub fn iter_print(&self) -> impl Iterator<Item = Arc<RwLock<Print>>> + '_ {
        let values: Vec<Arc<RwLock<Print>>> = self
            .print
            .read()
            .unwrap()
            .values()
            .map(|print| print.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`RangeExpression`] into the store.
    ///
    pub fn inter_range_expression(&mut self, range_expression: Arc<RwLock<RangeExpression>>) {
        let read = range_expression.read().unwrap();
        self.range_expression
            .write()
            .unwrap()
            .insert(read.id, range_expression.clone());
    }

    /// Exhume (get) [`RangeExpression`] from the store.
    ///
    pub fn exhume_range_expression(&self, id: &Uuid) -> Option<Arc<RwLock<RangeExpression>>> {
        self.range_expression
            .read()
            .unwrap()
            .get(id)
            .map(|range_expression| range_expression.clone())
    }

    /// Exorcise (remove) [`RangeExpression`] from the store.
    ///
    pub fn exorcise_range_expression(&mut self, id: &Uuid) -> Option<Arc<RwLock<RangeExpression>>> {
        self.range_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|range_expression| range_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, RangeExpression>`.
    ///
    pub fn iter_range_expression(&self) -> impl Iterator<Item = Arc<RwLock<RangeExpression>>> + '_ {
        let values: Vec<Arc<RwLock<RangeExpression>>> = self
            .range_expression
            .read()
            .unwrap()
            .values()
            .map(|range_expression| range_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Reference`] into the store.
    ///
    pub fn inter_reference(&mut self, reference: Arc<RwLock<Reference>>) {
        let read = reference.read().unwrap();
        self.reference
            .write()
            .unwrap()
            .insert(read.id, reference.clone());
    }

    /// Exhume (get) [`Reference`] from the store.
    ///
    pub fn exhume_reference(&self, id: &Uuid) -> Option<Arc<RwLock<Reference>>> {
        self.reference
            .read()
            .unwrap()
            .get(id)
            .map(|reference| reference.clone())
    }

    /// Exorcise (remove) [`Reference`] from the store.
    ///
    pub fn exorcise_reference(&mut self, id: &Uuid) -> Option<Arc<RwLock<Reference>>> {
        self.reference
            .write()
            .unwrap()
            .remove(id)
            .map(|reference| reference.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Reference>`.
    ///
    pub fn iter_reference(&self) -> impl Iterator<Item = Arc<RwLock<Reference>>> + '_ {
        let values: Vec<Arc<RwLock<Reference>>> = self
            .reference
            .read()
            .unwrap()
            .values()
            .map(|reference| reference.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ResultStatement`] into the store.
    ///
    pub fn inter_result_statement(&mut self, result_statement: Arc<RwLock<ResultStatement>>) {
        let read = result_statement.read().unwrap();
        self.result_statement
            .write()
            .unwrap()
            .insert(read.id, result_statement.clone());
    }

    /// Exhume (get) [`ResultStatement`] from the store.
    ///
    pub fn exhume_result_statement(&self, id: &Uuid) -> Option<Arc<RwLock<ResultStatement>>> {
        self.result_statement
            .read()
            .unwrap()
            .get(id)
            .map(|result_statement| result_statement.clone())
    }

    /// Exorcise (remove) [`ResultStatement`] from the store.
    ///
    pub fn exorcise_result_statement(&mut self, id: &Uuid) -> Option<Arc<RwLock<ResultStatement>>> {
        self.result_statement
            .write()
            .unwrap()
            .remove(id)
            .map(|result_statement| result_statement.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ResultStatement>`.
    ///
    pub fn iter_result_statement(&self) -> impl Iterator<Item = Arc<RwLock<ResultStatement>>> + '_ {
        let values: Vec<Arc<RwLock<ResultStatement>>> = self
            .result_statement
            .read()
            .unwrap()
            .values()
            .map(|result_statement| result_statement.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`XReturn`] into the store.
    ///
    pub fn inter_x_return(&mut self, x_return: Arc<RwLock<XReturn>>) {
        let read = x_return.read().unwrap();
        self.x_return
            .write()
            .unwrap()
            .insert(read.id, x_return.clone());
    }

    /// Exhume (get) [`XReturn`] from the store.
    ///
    pub fn exhume_x_return(&self, id: &Uuid) -> Option<Arc<RwLock<XReturn>>> {
        self.x_return
            .read()
            .unwrap()
            .get(id)
            .map(|x_return| x_return.clone())
    }

    /// Exorcise (remove) [`XReturn`] from the store.
    ///
    pub fn exorcise_x_return(&mut self, id: &Uuid) -> Option<Arc<RwLock<XReturn>>> {
        self.x_return
            .write()
            .unwrap()
            .remove(id)
            .map(|x_return| x_return.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, XReturn>`.
    ///
    pub fn iter_x_return(&self) -> impl Iterator<Item = Arc<RwLock<XReturn>>> + '_ {
        let values: Vec<Arc<RwLock<XReturn>>> = self
            .x_return
            .read()
            .unwrap()
            .values()
            .map(|x_return| x_return.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ZSome`] into the store.
    ///
    pub fn inter_z_some(&mut self, z_some: Arc<RwLock<ZSome>>) {
        let read = z_some.read().unwrap();
        self.z_some.write().unwrap().insert(read.id, z_some.clone());
    }

    /// Exhume (get) [`ZSome`] from the store.
    ///
    pub fn exhume_z_some(&self, id: &Uuid) -> Option<Arc<RwLock<ZSome>>> {
        self.z_some
            .read()
            .unwrap()
            .get(id)
            .map(|z_some| z_some.clone())
    }

    /// Exorcise (remove) [`ZSome`] from the store.
    ///
    pub fn exorcise_z_some(&mut self, id: &Uuid) -> Option<Arc<RwLock<ZSome>>> {
        self.z_some
            .write()
            .unwrap()
            .remove(id)
            .map(|z_some| z_some.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ZSome>`.
    ///
    pub fn iter_z_some(&self) -> impl Iterator<Item = Arc<RwLock<ZSome>>> + '_ {
        let values: Vec<Arc<RwLock<ZSome>>> = self
            .z_some
            .read()
            .unwrap()
            .values()
            .map(|z_some| z_some.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Span`] into the store.
    ///
    pub fn inter_span(&mut self, span: Arc<RwLock<Span>>) {
        let read = span.read().unwrap();
        self.span.write().unwrap().insert(read.id, span.clone());
    }

    /// Exhume (get) [`Span`] from the store.
    ///
    pub fn exhume_span(&self, id: &Uuid) -> Option<Arc<RwLock<Span>>> {
        self.span.read().unwrap().get(id).map(|span| span.clone())
    }

    /// Exorcise (remove) [`Span`] from the store.
    ///
    pub fn exorcise_span(&mut self, id: &Uuid) -> Option<Arc<RwLock<Span>>> {
        self.span
            .write()
            .unwrap()
            .remove(id)
            .map(|span| span.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Span>`.
    ///
    pub fn iter_span(&self) -> impl Iterator<Item = Arc<RwLock<Span>>> + '_ {
        let values: Vec<Arc<RwLock<Span>>> = self
            .span
            .read()
            .unwrap()
            .values()
            .map(|span| span.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Statement`] into the store.
    ///
    pub fn inter_statement(&mut self, statement: Arc<RwLock<Statement>>) {
        let read = statement.read().unwrap();
        self.statement
            .write()
            .unwrap()
            .insert(read.id, statement.clone());
    }

    /// Exhume (get) [`Statement`] from the store.
    ///
    pub fn exhume_statement(&self, id: &Uuid) -> Option<Arc<RwLock<Statement>>> {
        self.statement
            .read()
            .unwrap()
            .get(id)
            .map(|statement| statement.clone())
    }

    /// Exorcise (remove) [`Statement`] from the store.
    ///
    pub fn exorcise_statement(&mut self, id: &Uuid) -> Option<Arc<RwLock<Statement>>> {
        self.statement
            .write()
            .unwrap()
            .remove(id)
            .map(|statement| statement.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Statement>`.
    ///
    pub fn iter_statement(&self) -> impl Iterator<Item = Arc<RwLock<Statement>>> + '_ {
        let values: Vec<Arc<RwLock<Statement>>> = self
            .statement
            .read()
            .unwrap()
            .values()
            .map(|statement| statement.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`StaticMethodCall`] into the store.
    ///
    pub fn inter_static_method_call(&mut self, static_method_call: Arc<RwLock<StaticMethodCall>>) {
        let read = static_method_call.read().unwrap();
        self.static_method_call
            .write()
            .unwrap()
            .insert(read.id, static_method_call.clone());
    }

    /// Exhume (get) [`StaticMethodCall`] from the store.
    ///
    pub fn exhume_static_method_call(&self, id: &Uuid) -> Option<Arc<RwLock<StaticMethodCall>>> {
        self.static_method_call
            .read()
            .unwrap()
            .get(id)
            .map(|static_method_call| static_method_call.clone())
    }

    /// Exorcise (remove) [`StaticMethodCall`] from the store.
    ///
    pub fn exorcise_static_method_call(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<StaticMethodCall>>> {
        self.static_method_call
            .write()
            .unwrap()
            .remove(id)
            .map(|static_method_call| static_method_call.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, StaticMethodCall>`.
    ///
    pub fn iter_static_method_call(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<StaticMethodCall>>> + '_ {
        let values: Vec<Arc<RwLock<StaticMethodCall>>> = self
            .static_method_call
            .read()
            .unwrap()
            .values()
            .map(|static_method_call| static_method_call.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`StringLiteral`] into the store.
    ///
    pub fn inter_string_literal(&mut self, string_literal: Arc<RwLock<StringLiteral>>) {
        let read = string_literal.read().unwrap();
        self.string_literal
            .write()
            .unwrap()
            .insert(read.id, string_literal.clone());
    }

    /// Exhume (get) [`StringLiteral`] from the store.
    ///
    pub fn exhume_string_literal(&self, id: &Uuid) -> Option<Arc<RwLock<StringLiteral>>> {
        self.string_literal
            .read()
            .unwrap()
            .get(id)
            .map(|string_literal| string_literal.clone())
    }

    /// Exorcise (remove) [`StringLiteral`] from the store.
    ///
    pub fn exorcise_string_literal(&mut self, id: &Uuid) -> Option<Arc<RwLock<StringLiteral>>> {
        self.string_literal
            .write()
            .unwrap()
            .remove(id)
            .map(|string_literal| string_literal.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, StringLiteral>`.
    ///
    pub fn iter_string_literal(&self) -> impl Iterator<Item = Arc<RwLock<StringLiteral>>> + '_ {
        let values: Vec<Arc<RwLock<StringLiteral>>> = self
            .string_literal
            .read()
            .unwrap()
            .values()
            .map(|string_literal| string_literal.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`WoogStruct`] into the store.
    ///
    pub fn inter_woog_struct(&mut self, woog_struct: Arc<RwLock<WoogStruct>>) {
        let read = woog_struct.read().unwrap();
        self.woog_struct_id_by_name
            .write()
            .unwrap()
            .insert(read.name.to_upper_camel_case(), read.id);
        self.woog_struct
            .write()
            .unwrap()
            .insert(read.id, woog_struct.clone());
    }

    /// Exhume (get) [`WoogStruct`] from the store.
    ///
    pub fn exhume_woog_struct(&self, id: &Uuid) -> Option<Arc<RwLock<WoogStruct>>> {
        self.woog_struct
            .read()
            .unwrap()
            .get(id)
            .map(|woog_struct| woog_struct.clone())
    }

    /// Exorcise (remove) [`WoogStruct`] from the store.
    ///
    pub fn exorcise_woog_struct(&mut self, id: &Uuid) -> Option<Arc<RwLock<WoogStruct>>> {
        self.woog_struct
            .write()
            .unwrap()
            .remove(id)
            .map(|woog_struct| woog_struct.clone())
    }

    /// Exhume [`WoogStruct`] id from the store by name.
    ///
    pub fn exhume_woog_struct_id_by_name(&self, name: &str) -> Option<Uuid> {
        self.woog_struct_id_by_name
            .read()
            .unwrap()
            .get(name)
            .map(|id| *id)
    }

    /// Get an iterator over the internal `HashMap<&Uuid, WoogStruct>`.
    ///
    pub fn iter_woog_struct(&self) -> impl Iterator<Item = Arc<RwLock<WoogStruct>>> + '_ {
        let values: Vec<Arc<RwLock<WoogStruct>>> = self
            .woog_struct
            .read()
            .unwrap()
            .values()
            .map(|woog_struct| woog_struct.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`StructExpression`] into the store.
    ///
    pub fn inter_struct_expression(&mut self, struct_expression: Arc<RwLock<StructExpression>>) {
        let read = struct_expression.read().unwrap();
        self.struct_expression
            .write()
            .unwrap()
            .insert(read.id, struct_expression.clone());
    }

    /// Exhume (get) [`StructExpression`] from the store.
    ///
    pub fn exhume_struct_expression(&self, id: &Uuid) -> Option<Arc<RwLock<StructExpression>>> {
        self.struct_expression
            .read()
            .unwrap()
            .get(id)
            .map(|struct_expression| struct_expression.clone())
    }

    /// Exorcise (remove) [`StructExpression`] from the store.
    ///
    pub fn exorcise_struct_expression(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<StructExpression>>> {
        self.struct_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|struct_expression| struct_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, StructExpression>`.
    ///
    pub fn iter_struct_expression(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<StructExpression>>> + '_ {
        let values: Vec<Arc<RwLock<StructExpression>>> = self
            .struct_expression
            .read()
            .unwrap()
            .values()
            .map(|struct_expression| struct_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`XValue`] into the store.
    ///
    pub fn inter_x_value(&mut self, x_value: Arc<RwLock<XValue>>) {
        let read = x_value.read().unwrap();
        self.x_value
            .write()
            .unwrap()
            .insert(read.id, x_value.clone());
    }

    /// Exhume (get) [`XValue`] from the store.
    ///
    pub fn exhume_x_value(&self, id: &Uuid) -> Option<Arc<RwLock<XValue>>> {
        self.x_value
            .read()
            .unwrap()
            .get(id)
            .map(|x_value| x_value.clone())
    }

    /// Exorcise (remove) [`XValue`] from the store.
    ///
    pub fn exorcise_x_value(&mut self, id: &Uuid) -> Option<Arc<RwLock<XValue>>> {
        self.x_value
            .write()
            .unwrap()
            .remove(id)
            .map(|x_value| x_value.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, XValue>`.
    ///
    pub fn iter_x_value(&self) -> impl Iterator<Item = Arc<RwLock<XValue>>> + '_ {
        let values: Vec<Arc<RwLock<XValue>>> = self
            .x_value
            .read()
            .unwrap()
            .values()
            .map(|x_value| x_value.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`ValueType`] into the store.
    ///
    pub fn inter_value_type(&mut self, value_type: Arc<RwLock<ValueType>>) {
        let read = value_type.read().unwrap();
        self.value_type
            .write()
            .unwrap()
            .insert(read.id(), value_type.clone());
    }

    /// Exhume (get) [`ValueType`] from the store.
    ///
    pub fn exhume_value_type(&self, id: &Uuid) -> Option<Arc<RwLock<ValueType>>> {
        self.value_type
            .read()
            .unwrap()
            .get(id)
            .map(|value_type| value_type.clone())
    }

    /// Exorcise (remove) [`ValueType`] from the store.
    ///
    pub fn exorcise_value_type(&mut self, id: &Uuid) -> Option<Arc<RwLock<ValueType>>> {
        self.value_type
            .write()
            .unwrap()
            .remove(id)
            .map(|value_type| value_type.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, ValueType>`.
    ///
    pub fn iter_value_type(&self) -> impl Iterator<Item = Arc<RwLock<ValueType>>> + '_ {
        let values: Vec<Arc<RwLock<ValueType>>> = self
            .value_type
            .read()
            .unwrap()
            .values()
            .map(|value_type| value_type.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Variable`] into the store.
    ///
    pub fn inter_variable(&mut self, variable: Arc<RwLock<Variable>>) {
        let read = variable.read().unwrap();
        self.variable
            .write()
            .unwrap()
            .insert(read.id, variable.clone());
    }

    /// Exhume (get) [`Variable`] from the store.
    ///
    pub fn exhume_variable(&self, id: &Uuid) -> Option<Arc<RwLock<Variable>>> {
        self.variable
            .read()
            .unwrap()
            .get(id)
            .map(|variable| variable.clone())
    }

    /// Exorcise (remove) [`Variable`] from the store.
    ///
    pub fn exorcise_variable(&mut self, id: &Uuid) -> Option<Arc<RwLock<Variable>>> {
        self.variable
            .write()
            .unwrap()
            .remove(id)
            .map(|variable| variable.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Variable>`.
    ///
    pub fn iter_variable(&self) -> impl Iterator<Item = Arc<RwLock<Variable>>> + '_ {
        let values: Vec<Arc<RwLock<Variable>>> = self
            .variable
            .read()
            .unwrap()
            .values()
            .map(|variable| variable.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`VariableExpression`] into the store.
    ///
    pub fn inter_variable_expression(
        &mut self,
        variable_expression: Arc<RwLock<VariableExpression>>,
    ) {
        let read = variable_expression.read().unwrap();
        self.variable_expression
            .write()
            .unwrap()
            .insert(read.id, variable_expression.clone());
    }

    /// Exhume (get) [`VariableExpression`] from the store.
    ///
    pub fn exhume_variable_expression(&self, id: &Uuid) -> Option<Arc<RwLock<VariableExpression>>> {
        self.variable_expression
            .read()
            .unwrap()
            .get(id)
            .map(|variable_expression| variable_expression.clone())
    }

    /// Exorcise (remove) [`VariableExpression`] from the store.
    ///
    pub fn exorcise_variable_expression(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<VariableExpression>>> {
        self.variable_expression
            .write()
            .unwrap()
            .remove(id)
            .map(|variable_expression| variable_expression.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, VariableExpression>`.
    ///
    pub fn iter_variable_expression(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<VariableExpression>>> + '_ {
        let values: Vec<Arc<RwLock<VariableExpression>>> = self
            .variable_expression
            .read()
            .unwrap()
            .values()
            .map(|variable_expression| variable_expression.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"lu_dog-object-store-persistence"}}}
    /// Persist the store.
    ///
    /// The store is persisted as a a bincode file.
    pub fn persist_bincode<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        let mut bin_file = fs::File::create(path)?;
        let encoded: Vec<u8> = bincode::serialize(&self).unwrap();
        bin_file.write_all(&encoded)?;
        Ok(())
    }

    /// Persist the store.
    ///
    /// The store is persisted as a directory of JSON files. The intention
    /// is that this directory can be checked into version control.
    /// In fact, I intend to add automagic git integration as an option.
    pub fn persist<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        fs::create_dir_all(path)?;

        let path = path.join("lu_dog.json");
        fs::create_dir_all(&path)?;

        // Persist Argument.
        {
            let path = path.join("argument");
            fs::create_dir_all(&path)?;
            for argument in self.argument.read().unwrap().values() {
                let path = path.join(format!("{}.json", argument.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &argument)?;
            }
        }

        // Persist Binary.
        {
            let path = path.join("binary");
            fs::create_dir_all(&path)?;
            for binary in self.binary.read().unwrap().values() {
                let path = path.join(format!("{}.json", binary.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &binary)?;
            }
        }

        // Persist Block.
        {
            let path = path.join("block");
            fs::create_dir_all(&path)?;
            for block in self.block.read().unwrap().values() {
                let path = path.join(format!("{}.json", block.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &block)?;
            }
        }

        // Persist Boolean Literal.
        {
            let path = path.join("boolean_literal");
            fs::create_dir_all(&path)?;
            for boolean_literal in self.boolean_literal.read().unwrap().values() {
                let path = path.join(format!("{}.json", boolean_literal.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &boolean_literal)?;
            }
        }

        // Persist Call.
        {
            let path = path.join("call");
            fs::create_dir_all(&path)?;
            for call in self.call.read().unwrap().values() {
                let path = path.join(format!("{}.json", call.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &call)?;
            }
        }

        // Persist Comparison.
        {
            let path = path.join("comparison");
            fs::create_dir_all(&path)?;
            for comparison in self.comparison.read().unwrap().values() {
                let path = path.join(format!("{}.json", comparison.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &comparison)?;
            }
        }

        // Persist Dwarf Source File.
        {
            let path = path.join("dwarf_source_file");
            fs::create_dir_all(&path)?;
            for dwarf_source_file in self.dwarf_source_file.read().unwrap().values() {
                let path = path.join(format!("{}.json", dwarf_source_file.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &dwarf_source_file)?;
            }
        }

        // Persist Error.
        {
            let path = path.join("error");
            fs::create_dir_all(&path)?;
            for error in self.error.read().unwrap().values() {
                let path = path.join(format!("{}.json", error.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &error)?;
            }
        }

        // Persist Error Expression.
        {
            let path = path.join("error_expression");
            fs::create_dir_all(&path)?;
            for error_expression in self.error_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", error_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &error_expression)?;
            }
        }

        // Persist Expression.
        {
            let path = path.join("expression");
            fs::create_dir_all(&path)?;
            for expression in self.expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", expression.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &expression)?;
            }
        }

        // Persist Expression Statement.
        {
            let path = path.join("expression_statement");
            fs::create_dir_all(&path)?;
            for expression_statement in self.expression_statement.read().unwrap().values() {
                let path = path.join(format!("{}.json", expression_statement.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &expression_statement)?;
            }
        }

        // Persist Field.
        {
            let path = path.join("field");
            fs::create_dir_all(&path)?;
            for field in self.field.read().unwrap().values() {
                let path = path.join(format!("{}.json", field.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &field)?;
            }
        }

        // Persist Field Access.
        {
            let path = path.join("field_access");
            fs::create_dir_all(&path)?;
            for field_access in self.field_access.read().unwrap().values() {
                let path = path.join(format!("{}.json", field_access.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &field_access)?;
            }
        }

        // Persist Field Expression.
        {
            let path = path.join("field_expression");
            fs::create_dir_all(&path)?;
            for field_expression in self.field_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", field_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &field_expression)?;
            }
        }

        // Persist Float Literal.
        {
            let path = path.join("float_literal");
            fs::create_dir_all(&path)?;
            for float_literal in self.float_literal.read().unwrap().values() {
                let path = path.join(format!("{}.json", float_literal.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &float_literal)?;
            }
        }

        // Persist For Loop.
        {
            let path = path.join("for_loop");
            fs::create_dir_all(&path)?;
            for for_loop in self.for_loop.read().unwrap().values() {
                let path = path.join(format!("{}.json", for_loop.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &for_loop)?;
            }
        }

        // Persist Function.
        {
            let path = path.join("function");
            fs::create_dir_all(&path)?;
            for function in self.function.read().unwrap().values() {
                let path = path.join(format!("{}.json", function.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &function)?;
            }
        }

        // Persist Grouped.
        {
            let path = path.join("grouped");
            fs::create_dir_all(&path)?;
            for grouped in self.grouped.read().unwrap().values() {
                let path = path.join(format!("{}.json", grouped.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &grouped)?;
            }
        }

        // Persist If.
        {
            let path = path.join("x_if");
            fs::create_dir_all(&path)?;
            for x_if in self.x_if.read().unwrap().values() {
                let path = path.join(format!("{}.json", x_if.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &x_if)?;
            }
        }

        // Persist Implementation.
        {
            let path = path.join("implementation");
            fs::create_dir_all(&path)?;
            for implementation in self.implementation.read().unwrap().values() {
                let path = path.join(format!("{}.json", implementation.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &implementation)?;
            }
        }

        // Persist Import.
        {
            let path = path.join("import");
            fs::create_dir_all(&path)?;
            for import in self.import.read().unwrap().values() {
                let path = path.join(format!("{}.json", import.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &import)?;
            }
        }

        // Persist Index.
        {
            let path = path.join("index");
            fs::create_dir_all(&path)?;
            for index in self.index.read().unwrap().values() {
                let path = path.join(format!("{}.json", index.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &index)?;
            }
        }

        // Persist Integer Literal.
        {
            let path = path.join("integer_literal");
            fs::create_dir_all(&path)?;
            for integer_literal in self.integer_literal.read().unwrap().values() {
                let path = path.join(format!("{}.json", integer_literal.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &integer_literal)?;
            }
        }

        // Persist Item.
        {
            let path = path.join("item");
            fs::create_dir_all(&path)?;
            for item in self.item.read().unwrap().values() {
                let path = path.join(format!("{}.json", item.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &item)?;
            }
        }

        // Persist Let Statement.
        {
            let path = path.join("let_statement");
            fs::create_dir_all(&path)?;
            for let_statement in self.let_statement.read().unwrap().values() {
                let path = path.join(format!("{}.json", let_statement.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &let_statement)?;
            }
        }

        // Persist List.
        {
            let path = path.join("list");
            fs::create_dir_all(&path)?;
            for list in self.list.read().unwrap().values() {
                let path = path.join(format!("{}.json", list.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &list)?;
            }
        }

        // Persist List Element.
        {
            let path = path.join("list_element");
            fs::create_dir_all(&path)?;
            for list_element in self.list_element.read().unwrap().values() {
                let path = path.join(format!("{}.json", list_element.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &list_element)?;
            }
        }

        // Persist List Expression.
        {
            let path = path.join("list_expression");
            fs::create_dir_all(&path)?;
            for list_expression in self.list_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", list_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &list_expression)?;
            }
        }

        // Persist Literal.
        {
            let path = path.join("literal");
            fs::create_dir_all(&path)?;
            for literal in self.literal.read().unwrap().values() {
                let path = path.join(format!("{}.json", literal.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &literal)?;
            }
        }

        // Persist Local Variable.
        {
            let path = path.join("local_variable");
            fs::create_dir_all(&path)?;
            for local_variable in self.local_variable.read().unwrap().values() {
                let path = path.join(format!("{}.json", local_variable.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &local_variable)?;
            }
        }

        // Persist Method Call.
        {
            let path = path.join("method_call");
            fs::create_dir_all(&path)?;
            for method_call in self.method_call.read().unwrap().values() {
                let path = path.join(format!("{}.json", method_call.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &method_call)?;
            }
        }

        // Persist Object Store.
        {
            let path = path.join("z_object_store");
            fs::create_dir_all(&path)?;
            for z_object_store in self.z_object_store.read().unwrap().values() {
                let path = path.join(format!("{}.json", z_object_store.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &z_object_store)?;
            }
        }

        // Persist Operator.
        {
            let path = path.join("operator");
            fs::create_dir_all(&path)?;
            for operator in self.operator.read().unwrap().values() {
                let path = path.join(format!("{}.json", operator.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &operator)?;
            }
        }

        // Persist Option.
        {
            let path = path.join("woog_option");
            fs::create_dir_all(&path)?;
            for woog_option in self.woog_option.read().unwrap().values() {
                let path = path.join(format!("{}.json", woog_option.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &woog_option)?;
            }
        }

        // Persist Parameter.
        {
            let path = path.join("parameter");
            fs::create_dir_all(&path)?;
            for parameter in self.parameter.read().unwrap().values() {
                let path = path.join(format!("{}.json", parameter.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &parameter)?;
            }
        }

        // Persist Print.
        {
            let path = path.join("print");
            fs::create_dir_all(&path)?;
            for print in self.print.read().unwrap().values() {
                let path = path.join(format!("{}.json", print.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &print)?;
            }
        }

        // Persist Range Expression.
        {
            let path = path.join("range_expression");
            fs::create_dir_all(&path)?;
            for range_expression in self.range_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", range_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &range_expression)?;
            }
        }

        // Persist Reference.
        {
            let path = path.join("reference");
            fs::create_dir_all(&path)?;
            for reference in self.reference.read().unwrap().values() {
                let path = path.join(format!("{}.json", reference.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &reference)?;
            }
        }

        // Persist Result Statement.
        {
            let path = path.join("result_statement");
            fs::create_dir_all(&path)?;
            for result_statement in self.result_statement.read().unwrap().values() {
                let path = path.join(format!("{}.json", result_statement.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &result_statement)?;
            }
        }

        // Persist Return.
        {
            let path = path.join("x_return");
            fs::create_dir_all(&path)?;
            for x_return in self.x_return.read().unwrap().values() {
                let path = path.join(format!("{}.json", x_return.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &x_return)?;
            }
        }

        // Persist Some.
        {
            let path = path.join("z_some");
            fs::create_dir_all(&path)?;
            for z_some in self.z_some.read().unwrap().values() {
                let path = path.join(format!("{}.json", z_some.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &z_some)?;
            }
        }

        // Persist Span.
        {
            let path = path.join("span");
            fs::create_dir_all(&path)?;
            for span in self.span.read().unwrap().values() {
                let path = path.join(format!("{}.json", span.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &span)?;
            }
        }

        // Persist Statement.
        {
            let path = path.join("statement");
            fs::create_dir_all(&path)?;
            for statement in self.statement.read().unwrap().values() {
                let path = path.join(format!("{}.json", statement.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &statement)?;
            }
        }

        // Persist Static Method Call.
        {
            let path = path.join("static_method_call");
            fs::create_dir_all(&path)?;
            for static_method_call in self.static_method_call.read().unwrap().values() {
                let path = path.join(format!("{}.json", static_method_call.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &static_method_call)?;
            }
        }

        // Persist String Literal.
        {
            let path = path.join("string_literal");
            fs::create_dir_all(&path)?;
            for string_literal in self.string_literal.read().unwrap().values() {
                let path = path.join(format!("{}.json", string_literal.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &string_literal)?;
            }
        }

        // Persist Struct.
        {
            let path = path.join("woog_struct");
            fs::create_dir_all(&path)?;
            for woog_struct in self.woog_struct.read().unwrap().values() {
                let path = path.join(format!("{}.json", woog_struct.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &woog_struct)?;
            }
        }

        // Persist Struct Expression.
        {
            let path = path.join("struct_expression");
            fs::create_dir_all(&path)?;
            for struct_expression in self.struct_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", struct_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &struct_expression)?;
            }
        }

        // Persist Value.
        {
            let path = path.join("x_value");
            fs::create_dir_all(&path)?;
            for x_value in self.x_value.read().unwrap().values() {
                let path = path.join(format!("{}.json", x_value.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &x_value)?;
            }
        }

        // Persist Value Type.
        {
            let path = path.join("value_type");
            fs::create_dir_all(&path)?;
            for value_type in self.value_type.read().unwrap().values() {
                let path = path.join(format!("{}.json", value_type.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &value_type)?;
            }
        }

        // Persist Variable.
        {
            let path = path.join("variable");
            fs::create_dir_all(&path)?;
            for variable in self.variable.read().unwrap().values() {
                let path = path.join(format!("{}.json", variable.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &variable)?;
            }
        }

        // Persist Variable Expression.
        {
            let path = path.join("variable_expression");
            fs::create_dir_all(&path)?;
            for variable_expression in self.variable_expression.read().unwrap().values() {
                let path = path.join(format!("{}.json", variable_expression.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &variable_expression)?;
            }
        }

        Ok(())
    }

    /// Load the store.
    ///
    /// The store is as a bincode file.
    pub fn load_bincode<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let bin_file = fs::File::open(path)?;
        Ok(bincode::deserialize_from(bin_file).unwrap())
    }

    /// Load the store.
    ///
    /// The store is persisted as a directory of JSON files. The intention
    /// is that this directory can be checked into version control.
    /// In fact, I intend to add automagic git integration as an option.
    pub fn load<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let path = path.join("lu_dog.json");

        let mut store = Self::new();

        // Load Argument.
        {
            let path = path.join("argument");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let argument: Arc<RwLock<Argument>> = serde_json::from_reader(reader)?;
                store
                    .argument
                    .write()
                    .unwrap()
                    .insert(argument.read().unwrap().id, argument.clone());
            }
        }

        // Load Binary.
        {
            let path = path.join("binary");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let binary: Arc<RwLock<Binary>> = serde_json::from_reader(reader)?;
                store
                    .binary
                    .write()
                    .unwrap()
                    .insert(binary.read().unwrap().id(), binary.clone());
            }
        }

        // Load Block.
        {
            let path = path.join("block");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let block: Arc<RwLock<Block>> = serde_json::from_reader(reader)?;
                store
                    .block
                    .write()
                    .unwrap()
                    .insert(block.read().unwrap().id, block.clone());
            }
        }

        // Load Boolean Literal.
        {
            let path = path.join("boolean_literal");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let boolean_literal: Arc<RwLock<BooleanLiteral>> = serde_json::from_reader(reader)?;
                store.boolean_literal.write().unwrap().insert(
                    boolean_literal.read().unwrap().id(),
                    boolean_literal.clone(),
                );
            }
        }

        // Load Call.
        {
            let path = path.join("call");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let call: Arc<RwLock<Call>> = serde_json::from_reader(reader)?;
                store
                    .call
                    .write()
                    .unwrap()
                    .insert(call.read().unwrap().id, call.clone());
            }
        }

        // Load Comparison.
        {
            let path = path.join("comparison");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let comparison: Arc<RwLock<Comparison>> = serde_json::from_reader(reader)?;
                store
                    .comparison
                    .write()
                    .unwrap()
                    .insert(comparison.read().unwrap().id(), comparison.clone());
            }
        }

        // Load Dwarf Source File.
        {
            let path = path.join("dwarf_source_file");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let dwarf_source_file: Arc<RwLock<DwarfSourceFile>> =
                    serde_json::from_reader(reader)?;
                store.dwarf_source_file.write().unwrap().insert(
                    dwarf_source_file.read().unwrap().id,
                    dwarf_source_file.clone(),
                );
            }
        }

        // Load Error.
        {
            let path = path.join("error");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let error: Arc<RwLock<Error>> = serde_json::from_reader(reader)?;
                store
                    .error
                    .write()
                    .unwrap()
                    .insert(error.read().unwrap().id(), error.clone());
            }
        }

        // Load Error Expression.
        {
            let path = path.join("error_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let error_expression: Arc<RwLock<ErrorExpression>> =
                    serde_json::from_reader(reader)?;
                store.error_expression.write().unwrap().insert(
                    error_expression.read().unwrap().id,
                    error_expression.clone(),
                );
            }
        }

        // Load Expression.
        {
            let path = path.join("expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let expression: Arc<RwLock<Expression>> = serde_json::from_reader(reader)?;
                store
                    .expression
                    .write()
                    .unwrap()
                    .insert(expression.read().unwrap().id(), expression.clone());
            }
        }

        // Load Expression Statement.
        {
            let path = path.join("expression_statement");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let expression_statement: Arc<RwLock<ExpressionStatement>> =
                    serde_json::from_reader(reader)?;
                store.expression_statement.write().unwrap().insert(
                    expression_statement.read().unwrap().id,
                    expression_statement.clone(),
                );
            }
        }

        // Load Field.
        {
            let path = path.join("field");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let field: Arc<RwLock<Field>> = serde_json::from_reader(reader)?;
                store
                    .field
                    .write()
                    .unwrap()
                    .insert(field.read().unwrap().id, field.clone());
            }
        }

        // Load Field Access.
        {
            let path = path.join("field_access");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let field_access: Arc<RwLock<FieldAccess>> = serde_json::from_reader(reader)?;
                store
                    .field_access
                    .write()
                    .unwrap()
                    .insert(field_access.read().unwrap().id, field_access.clone());
            }
        }

        // Load Field Expression.
        {
            let path = path.join("field_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let field_expression: Arc<RwLock<FieldExpression>> =
                    serde_json::from_reader(reader)?;
                store.field_expression.write().unwrap().insert(
                    field_expression.read().unwrap().id,
                    field_expression.clone(),
                );
            }
        }

        // Load Float Literal.
        {
            let path = path.join("float_literal");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let float_literal: Arc<RwLock<FloatLiteral>> = serde_json::from_reader(reader)?;
                store
                    .float_literal
                    .write()
                    .unwrap()
                    .insert(float_literal.read().unwrap().id, float_literal.clone());
            }
        }

        // Load For Loop.
        {
            let path = path.join("for_loop");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let for_loop: Arc<RwLock<ForLoop>> = serde_json::from_reader(reader)?;
                store
                    .for_loop
                    .write()
                    .unwrap()
                    .insert(for_loop.read().unwrap().id, for_loop.clone());
            }
        }

        // Load Function.
        {
            let path = path.join("function");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let function: Arc<RwLock<Function>> = serde_json::from_reader(reader)?;
                store.function_id_by_name.write().unwrap().insert(
                    function.read().unwrap().name.to_lower_camel_case(),
                    function.read().unwrap().id,
                );
                store
                    .function
                    .write()
                    .unwrap()
                    .insert(function.read().unwrap().id, function.clone());
            }
        }

        // Load Grouped.
        {
            let path = path.join("grouped");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let grouped: Arc<RwLock<Grouped>> = serde_json::from_reader(reader)?;
                store
                    .grouped
                    .write()
                    .unwrap()
                    .insert(grouped.read().unwrap().id, grouped.clone());
            }
        }

        // Load If.
        {
            let path = path.join("x_if");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let x_if: Arc<RwLock<XIf>> = serde_json::from_reader(reader)?;
                store
                    .x_if
                    .write()
                    .unwrap()
                    .insert(x_if.read().unwrap().id, x_if.clone());
            }
        }

        // Load Implementation.
        {
            let path = path.join("implementation");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let implementation: Arc<RwLock<Implementation>> = serde_json::from_reader(reader)?;
                store
                    .implementation
                    .write()
                    .unwrap()
                    .insert(implementation.read().unwrap().id, implementation.clone());
            }
        }

        // Load Import.
        {
            let path = path.join("import");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let import: Arc<RwLock<Import>> = serde_json::from_reader(reader)?;
                store
                    .import
                    .write()
                    .unwrap()
                    .insert(import.read().unwrap().id, import.clone());
            }
        }

        // Load Index.
        {
            let path = path.join("index");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let index: Arc<RwLock<Index>> = serde_json::from_reader(reader)?;
                store
                    .index
                    .write()
                    .unwrap()
                    .insert(index.read().unwrap().id, index.clone());
            }
        }

        // Load Integer Literal.
        {
            let path = path.join("integer_literal");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let integer_literal: Arc<RwLock<IntegerLiteral>> = serde_json::from_reader(reader)?;
                store
                    .integer_literal
                    .write()
                    .unwrap()
                    .insert(integer_literal.read().unwrap().id, integer_literal.clone());
            }
        }

        // Load Item.
        {
            let path = path.join("item");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let item: Arc<RwLock<Item>> = serde_json::from_reader(reader)?;
                store
                    .item
                    .write()
                    .unwrap()
                    .insert(item.read().unwrap().id, item.clone());
            }
        }

        // Load Let Statement.
        {
            let path = path.join("let_statement");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let let_statement: Arc<RwLock<LetStatement>> = serde_json::from_reader(reader)?;
                store
                    .let_statement
                    .write()
                    .unwrap()
                    .insert(let_statement.read().unwrap().id, let_statement.clone());
            }
        }

        // Load List.
        {
            let path = path.join("list");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let list: Arc<RwLock<List>> = serde_json::from_reader(reader)?;
                store
                    .list
                    .write()
                    .unwrap()
                    .insert(list.read().unwrap().id, list.clone());
            }
        }

        // Load List Element.
        {
            let path = path.join("list_element");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let list_element: Arc<RwLock<ListElement>> = serde_json::from_reader(reader)?;
                store
                    .list_element
                    .write()
                    .unwrap()
                    .insert(list_element.read().unwrap().id, list_element.clone());
            }
        }

        // Load List Expression.
        {
            let path = path.join("list_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let list_expression: Arc<RwLock<ListExpression>> = serde_json::from_reader(reader)?;
                store
                    .list_expression
                    .write()
                    .unwrap()
                    .insert(list_expression.read().unwrap().id, list_expression.clone());
            }
        }

        // Load Literal.
        {
            let path = path.join("literal");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let literal: Arc<RwLock<Literal>> = serde_json::from_reader(reader)?;
                store
                    .literal
                    .write()
                    .unwrap()
                    .insert(literal.read().unwrap().id(), literal.clone());
            }
        }

        // Load Local Variable.
        {
            let path = path.join("local_variable");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let local_variable: Arc<RwLock<LocalVariable>> = serde_json::from_reader(reader)?;
                store
                    .local_variable
                    .write()
                    .unwrap()
                    .insert(local_variable.read().unwrap().id, local_variable.clone());
            }
        }

        // Load Method Call.
        {
            let path = path.join("method_call");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let method_call: Arc<RwLock<MethodCall>> = serde_json::from_reader(reader)?;
                store
                    .method_call
                    .write()
                    .unwrap()
                    .insert(method_call.read().unwrap().id, method_call.clone());
            }
        }

        // Load Object Store.
        {
            let path = path.join("z_object_store");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let z_object_store: Arc<RwLock<ZObjectStore>> = serde_json::from_reader(reader)?;
                store
                    .z_object_store
                    .write()
                    .unwrap()
                    .insert(z_object_store.read().unwrap().id, z_object_store.clone());
            }
        }

        // Load Operator.
        {
            let path = path.join("operator");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let operator: Arc<RwLock<Operator>> = serde_json::from_reader(reader)?;
                store
                    .operator
                    .write()
                    .unwrap()
                    .insert(operator.read().unwrap().id, operator.clone());
            }
        }

        // Load Option.
        {
            let path = path.join("woog_option");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let woog_option: Arc<RwLock<WoogOption>> = serde_json::from_reader(reader)?;
                store
                    .woog_option
                    .write()
                    .unwrap()
                    .insert(woog_option.read().unwrap().id, woog_option.clone());
            }
        }

        // Load Parameter.
        {
            let path = path.join("parameter");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let parameter: Arc<RwLock<Parameter>> = serde_json::from_reader(reader)?;
                store
                    .parameter
                    .write()
                    .unwrap()
                    .insert(parameter.read().unwrap().id, parameter.clone());
            }
        }

        // Load Print.
        {
            let path = path.join("print");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let print: Arc<RwLock<Print>> = serde_json::from_reader(reader)?;
                store
                    .print
                    .write()
                    .unwrap()
                    .insert(print.read().unwrap().id, print.clone());
            }
        }

        // Load Range Expression.
        {
            let path = path.join("range_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let range_expression: Arc<RwLock<RangeExpression>> =
                    serde_json::from_reader(reader)?;
                store.range_expression.write().unwrap().insert(
                    range_expression.read().unwrap().id,
                    range_expression.clone(),
                );
            }
        }

        // Load Reference.
        {
            let path = path.join("reference");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let reference: Arc<RwLock<Reference>> = serde_json::from_reader(reader)?;
                store
                    .reference
                    .write()
                    .unwrap()
                    .insert(reference.read().unwrap().id, reference.clone());
            }
        }

        // Load Result Statement.
        {
            let path = path.join("result_statement");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let result_statement: Arc<RwLock<ResultStatement>> =
                    serde_json::from_reader(reader)?;
                store.result_statement.write().unwrap().insert(
                    result_statement.read().unwrap().id,
                    result_statement.clone(),
                );
            }
        }

        // Load Return.
        {
            let path = path.join("x_return");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let x_return: Arc<RwLock<XReturn>> = serde_json::from_reader(reader)?;
                store
                    .x_return
                    .write()
                    .unwrap()
                    .insert(x_return.read().unwrap().id, x_return.clone());
            }
        }

        // Load Some.
        {
            let path = path.join("z_some");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let z_some: Arc<RwLock<ZSome>> = serde_json::from_reader(reader)?;
                store
                    .z_some
                    .write()
                    .unwrap()
                    .insert(z_some.read().unwrap().id, z_some.clone());
            }
        }

        // Load Span.
        {
            let path = path.join("span");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let span: Arc<RwLock<Span>> = serde_json::from_reader(reader)?;
                store
                    .span
                    .write()
                    .unwrap()
                    .insert(span.read().unwrap().id, span.clone());
            }
        }

        // Load Statement.
        {
            let path = path.join("statement");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let statement: Arc<RwLock<Statement>> = serde_json::from_reader(reader)?;
                store
                    .statement
                    .write()
                    .unwrap()
                    .insert(statement.read().unwrap().id, statement.clone());
            }
        }

        // Load Static Method Call.
        {
            let path = path.join("static_method_call");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let static_method_call: Arc<RwLock<StaticMethodCall>> =
                    serde_json::from_reader(reader)?;
                store.static_method_call.write().unwrap().insert(
                    static_method_call.read().unwrap().id,
                    static_method_call.clone(),
                );
            }
        }

        // Load String Literal.
        {
            let path = path.join("string_literal");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let string_literal: Arc<RwLock<StringLiteral>> = serde_json::from_reader(reader)?;
                store
                    .string_literal
                    .write()
                    .unwrap()
                    .insert(string_literal.read().unwrap().id, string_literal.clone());
            }
        }

        // Load Struct.
        {
            let path = path.join("woog_struct");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let woog_struct: Arc<RwLock<WoogStruct>> = serde_json::from_reader(reader)?;
                store.woog_struct_id_by_name.write().unwrap().insert(
                    woog_struct.read().unwrap().name.to_upper_camel_case(),
                    woog_struct.read().unwrap().id,
                );
                store
                    .woog_struct
                    .write()
                    .unwrap()
                    .insert(woog_struct.read().unwrap().id, woog_struct.clone());
            }
        }

        // Load Struct Expression.
        {
            let path = path.join("struct_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let struct_expression: Arc<RwLock<StructExpression>> =
                    serde_json::from_reader(reader)?;
                store.struct_expression.write().unwrap().insert(
                    struct_expression.read().unwrap().id,
                    struct_expression.clone(),
                );
            }
        }

        // Load Value.
        {
            let path = path.join("x_value");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let x_value: Arc<RwLock<XValue>> = serde_json::from_reader(reader)?;
                store
                    .x_value
                    .write()
                    .unwrap()
                    .insert(x_value.read().unwrap().id, x_value.clone());
            }
        }

        // Load Value Type.
        {
            let path = path.join("value_type");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let value_type: Arc<RwLock<ValueType>> = serde_json::from_reader(reader)?;
                store
                    .value_type
                    .write()
                    .unwrap()
                    .insert(value_type.read().unwrap().id(), value_type.clone());
            }
        }

        // Load Variable.
        {
            let path = path.join("variable");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let variable: Arc<RwLock<Variable>> = serde_json::from_reader(reader)?;
                store
                    .variable
                    .write()
                    .unwrap()
                    .insert(variable.read().unwrap().id, variable.clone());
            }
        }

        // Load Variable Expression.
        {
            let path = path.join("variable_expression");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let variable_expression: Arc<RwLock<VariableExpression>> =
                    serde_json::from_reader(reader)?;
                store.variable_expression.write().unwrap().insert(
                    variable_expression.read().unwrap().id,
                    variable_expression.clone(),
                );
            }
        }

        Ok(store)
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
