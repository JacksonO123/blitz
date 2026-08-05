const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("blitz.zig");
const clone = blitz.clone;

pub const CommonError = error{
    ExpectedU64OrU32ForArrayDecSize,
    FailedToGetCustomInstanceById,
    CannotSetGenericToVarInfo,
} || Allocator.Error || std.fmt.ParseIntError;

pub const ScanError = error{
    // misc
    ScanStartedInLowerScope,
    InvalidCast,
    ExpectedBooleanBang,
    ExpectedBooleanIfCondition,
    UnsupportedFeature,
    ExpectedU64OrU32ForIndex,
    StaticStructInstanceCannotBeUsedAsVariable,
    InvalidNumber,
    IfStatementMayOnlyHaveOneElse,
    ElseBranchOutOfOrder,
    NestedVarInfoDetected,
    RawNumberTooBigForType,
    InvalidEqOperationType,

    // pointers
    PointerTypeMismatch,
    CannotDereferenceNonPointerValue,
    CannotTakePointerOfRawValue,
    CannotFreeNonPointerType,

    // arrays
    ArrayTypeMismatch,
    ExpectedArrayForIndexTarget,
    ArrayDecSizeMismatch,
    ArrayInitTypeInitializerMismatch,
    ExpectedSliceFoundArray,
    ExpectedArrayFoundSlice,
    CanOnlyMakeSliceFromSizedArray,

    // loops
    ExpectedBooleanLoopCondition,
    LoopControlFlowUsedOutsideOfLoop,

    // variables
    VariableAnnotationMismatch,
    VariableAlreadyExists,
    VoidVariableDec,
    VariableTypeAndValueTypeMismatch,
    AssigningToConstVariable,
    PointerTypeConstMismatch,
    StrictMutTypeMismatch,
    InvalidSetValueTarget,
    UndefVariableRequiresAnnotation,
    ValueSetTargetNotAVariable,

    // functions
    ExpectedFunctionReturn,
    FunctionCallParamTypeMismatch,
    FunctionCallParamCountMismatch,
    FunctionReturnTypeMismatch,
    IdentifierNotAFunction,
    CannotCallNonFunctionNode,
    VariableIsUndefined,
    FunctionNotInScope,
    FunctionReturnIsNotExhaustive,
    FunctionMissingReturn,
    UnexpectedReturnStatement,
    ExpectedMutableParameter,
    CallGenericsAndFuncDecGenericCountMismatch,
    UnexpectedCallGenerics,
    UnexpectedSelfParameter,
    ExpectedSelfParameterToBeFirst,
    ExpectedSelfParameter,
    CaptureVariableIsNotInScope,
    CaptureVariableConstMismatch,

    // structs
    GenericCountMismatch,
    StructInitAttributeCountMismatch,
    StructInitMemberTypeMismatch,
    StructInitAttributeNotFound,
    InvalidProperty,
    StaticAccessFromStructInstance,
    NonStaticAccessFromStaticStructReference,
    SelfUsedOutsideStruct,
    StructDoesNotExist,
    RestrictedPropertyAccess,
    InvalidPropertySource,
    NonPublicStructFieldAccessFromOutsideDefinition,
    GenericStructMethodRedefiningStructGeneric,
    ExpectedMutableStructInstance,

    // operations
    MathOpOnNonNumberType,
    MathOpTypeMismatch,
    ExpectedBoolInBoolOp,
    InvalidBitOperation,
    BitMaskWithMismatchingSize,
    NumberTypeMismatch,
    ComparisonOnNonNumberType,
    CannotIncDecNonNumberType,

    // generics
    EmptyGenericType,
    CustomGenericMismatch,
    ConflictingGenericParameters,
    GenericRestrictionConflict,
    UnexpectedRecursiveGeneric,
    GenericNotFound,

    // errors
    ExpectedUseOfErrorVariants,
    ErrorDoesNotHaveVariants,
    ErrorVariantDoesNotExist,

    // enums
    EnumVariantDoesNotExist,
} || CommonError;

pub const AstError = error{
    InvalidExprOperand,
    ExpectedExpression,
    ExpectedIdentifierForVariableName,
    ExpectedIdentifierForFunctionName,
    ExpectedIdentifierForParameterName,
    ExpectedIdentifierForGenericType,
    ExpectedIdentifierForErrorName,
    ExpectedIdentifierForPropertyAccess,
    ExpectedIdentifierForErrorVariant,
    ExpectedIdentifierForStructName,
    ExpectedNameForError,
    ExpectedNameForStruct,
    ExpectedNameForFunction,
    ExpectedSizeForArrayDec,
    ExpectedIdentifierForStructProperty,
    ExpectedValueForStructProperty,
    ExpectedIdentifierPropertyAccessSource,
    UnexpectedGenericOnErrorType,
    ExpectedTypeExpression,
    ErrorPayloadMayNotBeError,
    UnexpectedGeneric,
    UnexpectedMutSpecifierOnGeneric,
    ExpectedU64ForArraySize,
    StructDefinedInLowerScope,
    ErrorDefinedInLowerScope,
    FunctionDefinedInLowerScope,
    NegativeNumberWithUnsignedTypeConflict,
    ExpectedIdentifierForArrayInitIndex,
    ExpectedIdentifierForArrayInitPtr,
    SelfStructNameNotFound,
    UnexpectedSelfParamOnStaticFunction,
    ExpectedIdentifierForEnumName,
    ExpectedIdentifierForEnumVariant,
    EnumDefinedInLowerScope,
    ExpectedNameForEnum,
    StructMethodsCannotDefineCaptureGroups,
    EmptyFunctionCaptures,
    ExpectedUniqueStructDecAttribute,
} || CommonError;

pub const TokenizeError = error{
    NumberHasTwoPeriods,
    NoClosingQuote,
    ExpectedCharacterFoundNothing,
    UnexpectedCharacter,
    CharTokenTooLong,
    CharTokenTooShort,
};

pub const AstTokenError = error{
    ExpectedTokenFoundNothing,
    UnexpectedToken,
};

pub const CloneError = error{
    GenericNotFound,
    BadGenericClone,
    CannotCloneFunction,
    CannotCloneStructDec,
    CannotCloneErrorDec,
    CannotCloneEnumDec,
} || CommonError;

pub const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
    ExpectedLoopInfo,
    ImmediateValueTooLarge,
    RegInteractionNotSupported,
    NoTrivialRegister,
    AccessTargetDoesNotHaveStructName,
    LabelDoesNotExist,
    MainFunctionNotFound,
    ResultOfAccessRegNotFound,
};

pub const GenBytecodeError = CodeGenError || CommonError;
