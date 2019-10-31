BeginPackage["Monitoring`"];

DeployPipeline;

Begin["`Private`"];

$CurrentDirectory = FileNameDrop[$InputFileName, -2];

DeployPipeline[] := DeployPipeline[FileNameJoin[{$CurrentDirectory, "config.wl"}]]
DeployPipeline[configFile_] :=
    Module[{configData, cloudConfig, result},
        configData = ImportConfiguration[configFile];

        cloudConfig = URLBuild[{configData["root"], "config.wl"}];
        result = {DeployConfiguration[configData, cloudConfig]};
        If[TrueQ[MemberQ[result, _?FailureQ]],
            Return[$Failed]
        ];

        result = Join[result, DeployPackages[configData["root"]]];
        If[TrueQ[MemberQ[result, _?FailureQ]],
            Return[$Failed]
        ];

        result = Join[result, DeployDataTasks[configData, cloudConfig]];
        If[TrueQ[MemberQ[result, _?FailureQ]],
            Return[$Failed]
        ];

        result = Join[result, DeployImageTasks[configData, cloudConfig]];
        If[TrueQ[MemberQ[result, _?FailureQ]],
            Return[$Failed]
        ];

        result = Append[result, DeployDashboard[configData, cloudConfig]];
        If[TrueQ[MemberQ[result, _?FailureQ]],
            Return[$Failed]
        ];

        result
    ]

ImportConfiguration[file_] := ImportConfiguration[file, FileExtension[file]]
ImportConfiguration[file_, "wl"] := Get[file]
ImportConfiguration[file_, "json"] := Import[file, "JSON"]
ImportConfiguration[file_, "yml"] :=
    Module[{session, content},
        session = StartExternalSession["Python"];
        content = ResourceFunction["ImportYAML"][session, file];
        DeleteObject[session];
        content
    ]

DeployConfiguration[data_, cloudConfig_] := CloudPut[data, cloudConfig]

DeployPackages[root_] :=
    Module[{packages, packDir},
        packDir = FileNameJoin[{$CurrentDirectory, "Packages"}];
        packages = FileNames["*.wl", packDir, 2];
        Table[CopyFile[pack,CloudFile[packDir, URLBuild[{root, "Packages"}], pack]], {pack, packages}]
    ]

CloudFile[dir_, cloudDir_, pack_] :=
    CloudObject[URLBuild[{cloudDir, StringReplace[pack, dir -> ""]}]]

DeployDataTasks[config_, cloudConfig_] :=
    Table[
        DeployDataTask[config["root"], type, config["admins"], config["term"], cloudConfig],
        {type, {"Collection", "Processing"}}
    ]

DeployDataTask[root_, type_, admin_, term_, cloudConfig_] :=
    Module[{template, assoc, dest},
        template = FileNameJoin[{$CurrentDirectory, "Tasks", "data.task"}];
        assoc = <|
            "root" -> root,
            "type" -> type,
            "func" -> findDataFunc[type],
            "schedule" -> ToString[findDataSched[type]],
            "admin" -> admin,
            "config" -> cloudConfig
        |>;
        dest = URLBuild[{root, term, "Tasks", type<>".task"}];
        DeployTask[template, assoc, dest]
    ]

DeployTask[template_, assoc_, dest_] :=
    Module[{str, taskStrExpr, taskExpr},
        Check[
            str = Import[template, "String"];
            taskStrExpr = StringTemplate[str][assoc];
            taskExpr = ToExpression[taskStrExpr];
            CloudDeploy[
                taskExpr,
                dest
            ],
            Print[taskStrExpr];
            $Failed
        ]
    ]

findDataFunc["Collection"] := "Monitoring`GatherData";
findDataFunc["Processing"] := "Monitoring`ProcessData";

findDataSched["Collection"] := DateObject[{_, _, _, 0}]
findDataSched["Processing"] := DateObject[{_, _, _, 1}]

$Graphics = {"TimeSeries", "Geographic", "Histogram", "WordCloud"};

DeployImageTasks[config_, cloudConfig_] :=
    Table[
        DeployImageTask[config["root"], type, config["admins"], config["term"], cloudConfig],
        {type, $Graphics}
    ]

DeployImageTask[root_, type_, admin_, term_, cloudConfig_] :=
    Module[{template, assoc, dest},
        template = FileNameJoin[{$CurrentDirectory, "Tasks", "graphics.task"}];
        assoc = <|
            "root" -> root,
            "type" -> type,
            "func" -> findGraphicsFunc[type],
            "schedule" -> ToString[findGraphicsSched[type]],
            "admin" -> admin,
            "config" -> cloudConfig,
            "dest" -> URLBuild[{root, term, "Images", type, DateString[{"Year","-","Month","-","Day"}]}]
        |>;
        dest = URLBuild[{root, term, "Tasks", type<>".task"}];
        DeployTask[template, assoc, dest]
    ]

findGraphicsFunc["TimeSeries"] := "Monitoring`GenerateTimeSeries";
findGraphicsFunc["Geographic"] := "Monitoring`GenerateMap";
findGraphicsFunc["Histogram"] := "Monitoring`GenerateHistogram";
findGraphicsFunc["WordCloud"] := "Monitoring`GenerateWordCloud";

findGraphicsSched[_] := DateObject[{_, _, _, 3}]

DeployDashboard[config_, cloudConfig_] :=
    Module[{root = config["root"], term = config["term"], template, strExpr, expr},
        template = Import[FileNameJoin[{$CurrentDirectory, "Dashboards", "delayed.template"}], "String"];
        strExpr = StringTemplate[template][<|"root"->root, "term"->term|>];
        expr = ToExpression[strExpr];
        CloudDeploy[
            expr,
            URLBuild[{root, term, "Dashboard"}],
            Permissions -> "Public"
        ]
    ]

End[];

EndPackage[];