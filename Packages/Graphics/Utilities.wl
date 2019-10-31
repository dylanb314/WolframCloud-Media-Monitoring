BeginPackage["Monitoring`"];

GetData;
StyleSetting;

Begin["`Private`"];

GetData[file_] /; $CloudEvaluation := Import[file, "MX"]
GetData[file_] := CloudImport[file, "MX"]

StyleSetting["AspectRation"] = 1/4;
StyleSetting["ColorProfile"] = "Rainbow";
StyleSetting["FontFamily"] = "Roboto";
StyleSetting["TitleFontSize"] = 16;
StyleSetting["FontSize"] = 14;
StyleSetting["Theme"] = "Business";
StyleSetting["GeoBackground"] = "Satellite";
StyleSetting["GeoProjection"] = "Robinson";
StyleSetting["ImageSize"] = Full;

End[];

EndPackage[];