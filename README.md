# Slack backup

Small OCaml tool to backup channels or conversations from slack.

## Examples

```
$ slack-backup conversation --name slackbot > slackot.json
$ slack-backup channel --name #general > general.json
```

## Requirements

- slacko
- cmdliner
- result
