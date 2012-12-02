open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type CutoffDecision = 
    { Mode : string; Body : string; Success : bool }
    member this.Text = 
        if(this.Success) then "Inserted " + this.Mode + " statement of content " + this.Body 
        else "Cutted of " + this.Mode + " statement of content " + this.Body
    
type ReplaceDecision = 
    { ParameterName : string; ParameterValue : string; Success : bool }
    member this.Text = 
        if(this.Success) then System.String.Format("[OK] Replaced parameter '{0}' of value '{1}'", this.ParameterName, this.ParameterValue)
        else if (this.ParameterValue = null)
            then sprintf "[OK] Replaced parameter '%s' of value '%s'" this.ParameterName this.ParameterValue
            else sprintf "[WARNING] Parameter '%s' of value '%s' was not used in template" this.ParameterName this.ParameterValue

let getModeRegex() =
    let ready = 
        "[$][(][_][MODE][_][)][_][{][_][BODY][_][}]"
            .Replace("[$]", "\\$").Replace("[_]", "\\s*")
            .Replace("[(]", "\\(").Replace("[)]", "\\)")            
            .Replace("[{]", "\\{").Replace("[}]", "\\}")
            .Replace("[MODE]", "(?<mode>[a-zA-Z]+)").Replace("[BODY]", "(?<body>.*?)")
    new Regex(ready, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)

// вырезаем всё из другого режима
let cutoffModes (template:string, mode:string) : (string * list<CutoffDecision>) =     
    let decisions = new List<CutoffDecision>()    
    let eval = 
        new MatchEvaluator(
            fun m -> 
                if (m.Success && m.Groups.["mode"].Value = mode) then
                    decisions.Add({Success = true; Mode = m.Groups.["mode"].Value; Body = m.Groups.["body"].Value;})
                    m.Groups.["body"].Value; // return
                else        
                    decisions.Add({Success = false; Mode = m.Groups.["mode"].Value; Body = m.Groups.["body"].Value;})
                    "" // return
                    )

    getModeRegex().Replace(template, eval), [for i in decisions -> i] // return

let readParameters paramsFilePath =
    let content = File.ReadAllLines(paramsFilePath)
    Map.ofSeq( seq { for i in content -> (i.Split('=').[0].Trim(), i.Split('=').[1].Trim()) })    

let getParamRegex() =
    let patterns = "[$][{][_][PARAM][_][}]"
    let ready = 
        patterns.Replace("[$]", "\\$").Replace("[_]", "\\s*").Replace("[{]", "\\{").Replace("[PARAM]", "(?<param>[a-zA-Z0-9_.:]+)").Replace("[}]", "\\}")   
    new Regex(ready, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)

// заменяем параметры на их значения
let replaceParameters (template: string, paramz : Map<string, string>) =
    let decisions = new List<ReplaceDecision>()
    let usedParams : HashSet<string> = new HashSet<string>()
    let eval = 
        new MatchEvaluator(
            fun m -> 
                if (m.Success && paramz.ContainsKey(m.Groups.["param"].Value)) then
                    decisions.Add({Success = true; ParameterName = m.Groups.["param"].Value; ParameterValue = paramz.[m.Groups.["param"].Value]})
                    let _ = usedParams.Add(m.Groups.["param"].Value)
                    paramz.[m.Groups.["param"].Value] // return
                else        
                    decisions.Add({Success = false; ParameterName = m.Groups.["param"].Value; ParameterValue = null})
                    "" // return    
                    )
    let res = getParamRegex().Replace(template, eval)
    for i in paramz do
        if (usedParams.Contains(i.Key) = false) then     
            decisions.Add({Success = false; ParameterName = i.Key; ParameterValue = i.Value})            
            
    res, [for i in decisions -> i] // return  

let getOutputPath path =
    Path.Combine(Path.GetDirectoryName(path), Path.GetFileNameWithoutExtension(path))  

[<EntryPoint>]
let main argv =     
    if argv.Length = 0 then 
        printfn "<this.exe> MODE PATH/TO/TEMPLATE PATH/TO/PARAMS"
        0
    else
        let template = File.ReadAllText (path = argv.[1])       
        let template1, cuts = cutoffModes(template, argv.[0])
        let template2, repl = replaceParameters(template1, readParameters(argv.[2]))
        File.WriteAllText(getOutputPath(argv.[1]), template2)

        for i in cuts do printfn "%s" i.Text       
        for i in repl do printfn "%s" i.Text
        0 // return an integer exit code