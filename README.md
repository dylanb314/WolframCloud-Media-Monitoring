# WolframCloud-Media-Monitoring

A system for monitoring media mentions of specific key words/phrases using the [Webhose.io](https://webhose.io) API and deployed to the [Wolfram Cloud](https://www.wolframcloud.com).

[![View notebooks](https://wolfr.am/HAAhzkRq)](https://wolfr.am/HF7CBvPF)

## Setup

To set this up, simply make a copy of `config.wl.template` and fill it out with your relevant information, i.e. desired cloud, username/password, webhose token, etc.

You may also want to look at the various task and dashboard configuration files to confirm that you will have the desired data shown on your desired schedule.

## Deployment

To deploy this to a Cloud, execute the following WL code (substituting the correct file paths):
```
Get["{repo-path}/Packages/Deployment.wl"];

ConnectAccount[]

DeployPipeline[]
```

## Webhose

[Webhose Reviews Documentation](https://docs.webhose.io/docs/reviews-api)
