open FSCL.Compiler
open FSCL.Language

[<ReflectedDefinition;Kernel>]
let add(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]

[<EntryPoint>]
let main _ = 
    let ws = new WorkSize(1024L, 64L)
    let compiler =  new Compiler()
    let a, b, c = [| 1.0f; 2.0f; 3.0f; 4.0f |], [| 1.0f; 2.0f; 3.0f; 4.0f |], [| 0.0f; 0.0f; 0.0f; 0.0f |]
    let compilerResult = compiler.Compile(<@ add(a, b, c, ws) @>)
    let e = compilerResult :?> IKernelExpression
    let code = (e.KFGRoot :?> KFGKernelNode).Module.Code
    printfn "%s" <| code.Value.Trim()
    0
