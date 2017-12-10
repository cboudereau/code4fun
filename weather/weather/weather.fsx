#r @"D:\packages\FSharp.Data\lib\net45\FSharp.Data.dll"
#I @"D:\packages\Google.DataTable.Net.Wrapper\lib\"
#I @"D:\packages\XPlot.GoogleCharts\lib\net45\"
#r "XPlot.GoogleCharts.dll"

open System
open FSharp.Data

type VerifCom = HtmlProvider< "https://www.verif.com/Hit-parade/03-Defaillance/01-Par-departement/75-Paris" >

VerifCom.GetSample().Tables.``Classement des entreprises défaillantes du département Paris 2``.Rows
|> Array.map(fun x -> x.``Raison sociale``, x.Défaillance, x.``C.A.``)


open XPlot.GoogleCharts

type ParametersApi = JsonProvider< "parameters.json" >

type WeatherApi = JsonProvider< "weather.json" >

//let weather = WeatherApi.Load("""weather.json""")
let weather = WeatherApi.Load("""70ab4b74ebeddf95601c560f275ce376.json""")
let ``1970`` = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
let epoch x = ``1970``.AddSeconds(float x)

let (minDt, maxDt) = 
    weather
    |> Array.fold (fun (mn,mx) x -> 
        let mn = min mn (epoch x.Dt)
        let mx = max mx (epoch x.Dt)
        (mn, mx)) (DateTime.MaxValue, DateTime.MinValue)

let tempMins = 
    weather 
    //|> Array.filter (fun x -> x.Main.TempMin > 0m && x.Main.TempMin < 500m) 
    |> Array.map(fun x -> epoch x.Dt, x.Main.TempMin)

tempMins
|> Chart.Line
|> Chart.Show

let rain (x:WeatherApi.Root) = 
    let rain = x.Rain

    rain 
    |> Option.bind (fun x -> x.``1h``)
    |> Option.orElse (rain |> Option.bind (fun x -> x.``3h``))
    |> Option.defaultValue 0m

let pressure (x:WeatherApi.Root) = x.Main.Pressure 

let options = 
    let o = Configuration.Options()
    o.dataOpacity <- 0.20
    o.pointSize <- 10
    o

weather 
|> Array.map (fun x -> rain x, pressure x)
|> Array.filter (snd >> (<>) 0)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "Rain"
|> Chart.WithYTitle "Pressure"
|> Chart.Show

weather
|> Array.map(fun x -> x.Main.Humidity, x.Wind.Speed)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "Humidity"
|> Chart.WithYTitle "Wind speed"
|> Chart.Show
