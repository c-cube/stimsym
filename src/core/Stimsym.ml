
module Document = Document
module Completion = Completion
module Eval = Eval
module Expr = Expr
module Parser = Parser
module Printer = Printer
module Subst = Subst
module Builtins = Builtins
module Builtins_advanced = Builtins_advanced
module Lexer = Lexer
module Parse_loc = Parse_loc

let init () =
  ignore (Sys.opaque_identity Builtins_advanced.init ())
