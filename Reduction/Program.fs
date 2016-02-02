open FSCL
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime

[<ReflectedDefinition;Kernel>]
let reductionVector(dataIn: float4[],
                    dataOut: float4[],
                    [<AddressSpace(AddressSpace.Local)>]
                    partialSums: float4[],
                    wi: WorkItemInfo) =

    let globalId = wi.GlobalID(0)
    let localId = wi.LocalID(0)
    let workGroupId = wi.GroupID(0)
    let workGroupSize = wi.LocalSize(0)

    partialSums.[localId] <- dataIn.[globalId]
    wi.LocalBarrier()

    let mutable i = workGroupSize >>> 1
    while i > 0 do
        if localId < i then partialSums.[localId] <- partialSums.[localId] + partialSums.[localId + i]
        wi.LocalBarrier()
        i <- i >>> 1

    if localId = 0 then dataOut.[workGroupId] <- partialSums.[0]

[<ReflectedDefinition;Kernel>]
let reductionComplete(  data: float4[],
                        [<AddressSpace(AddressSpace.Local)>]
                        partialSums: float4[],
                        sum: float32[],
                        wi: WorkItemInfo) =

    let localId = wi.LocalID(0)
    let workGroupSize = wi.LocalSize(0)

    partialSums.[localId] <- data.[localId]
    wi.LocalBarrier()

    let mutable i = workGroupSize >>> 1
    while i > 0 do
        if localId < i then partialSums.[localId] <- partialSums.[localId] + partialSums.[localId + i]
        wi.LocalBarrier()
        i <- i >>> 1

    if localId = 0 then
        let ps0 = partialSums.[0]
        sum.[0] <- ps0.x + ps0.y + ps0.z + ps0.w

[<EntryPoint>]
let main _ = 
    let NUM_FLOATS = 1024 * 1024
    let NUM_FLOATS_PER_WORK_ITEM = 4
    let NUM_WORK_ITEMS = NUM_FLOATS / NUM_FLOATS_PER_WORK_ITEM
    let WORK_GROUP_SIZE = 32
    let NUM_WORK_GROUPS = NUM_WORK_ITEMS / WORK_GROUP_SIZE
    let VALUE = 42.0f
    let VALUE4 = float4(VALUE, VALUE, VALUE, VALUE)


    let data1: float4[] = Array.replicate NUM_WORK_ITEMS VALUE4
    let data2: float4[] = Array.zeroCreate (WORK_GROUP_SIZE * NUM_WORK_GROUPS)
    let partialSums: float4[] = Array.zeroCreate WORK_GROUP_SIZE
    let sum: float32[] = Array.zeroCreate 1

    // let compiler =  new Compiler()
    // let compilerResult1 = compiler.Compile(<@ reductionVector(data1, data2, partialSums, ws) @>)
    // let e1 = compilerResult1 :?> IKernelExpression
    // let code1 = (e1.KFGRoot :?> KFGKernelNode).Module.Code
    // printfn "%s" <| code1.Value.Trim()

    // let platforms = OpenCL.OpenCLPlatform.Platforms
    // let platform = platforms.[0]

    let ws1 = new WorkSize((int64)NUM_WORK_ITEMS, (int64)WORK_GROUP_SIZE)
    let mutable globalWorkSize = NUM_WORK_ITEMS
    let mutable index = 0
    let mutable dataResult = data2
    while globalWorkSize >= WORK_GROUP_SIZE do
        let dataIn = if index % 2 = 0 then data1 else data2
        let dataOut = if index % 2 = 0 then data2 else data1
        dataResult <- dataOut
        <@ reductionVector(dataIn, dataOut, partialSums, ws1) @>.Run()
        globalWorkSize <- globalWorkSize / WORK_GROUP_SIZE
        index <- index + 1

    let ws2 = new WorkSize((int64)globalWorkSize, (int64)globalWorkSize)
    <@ reductionComplete(dataResult, partialSums, sum, ws2) @>.Run()

    printfn "OpenCL answer: %A" sum.[0]

    0
