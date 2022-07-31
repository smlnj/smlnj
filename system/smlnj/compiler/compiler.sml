(* compiler/compiler.sml
 * 
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure Compiler = struct
    structure Stats = Stats
    structure Control = Control
    structure Source = Source
    structure SourceMap = SourceMap
    structure ErrorMsg = ErrorMsg
    structure Symbol = Symbol
    structure SymPath = SymPath
    structure StaticEnv = StaticEnv
    structure DynamicEnv = DynamicEnv
    structure Environment = Environment
    structure EnvRef = EnvRef
    structure ModuleId = ModuleId
    structure PersStamps = PersStamps
    structure PrettyPrint = PrettyPrint
    structure PPTable =	CompilerPPTable
    structure Ast = Ast
    structure SmlFile = SmlFile
    structure Rehash = Rehash
    structure PrintHooks = PrintHooks
    structure Version = SMLNJVersion

    open Backend

    val version = Version.version
end
