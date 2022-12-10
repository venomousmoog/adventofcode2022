param ([int] $nday)

$day = '{0:d2}' -f $nday
$root = $PSScriptRoot
$day1 = "$root\day01"


New-Item "$root\day$day" -ItemType Directory
New-Item "$root\day$day\Properties" -ItemType Directory

@("day01.fsproj", "day01.fs","data.txt","test.txt","Properties\launchSettings.json") | Foreach-Object {
    (Get-Content "$day1\$_").Replace("day01", "day$day") 
        | Set-Content "$root\day$day\$_".Replace("day01", "day$day")
}
dotnet sln "$root\adventofcode2022.sln" add "$root\day$day\day$day.fsproj"