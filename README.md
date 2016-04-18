# Slack backup

Small OCaml tool to backup channels or conversations from slack.

## Examples

```
$ slack-backup conversation --name khady > khady.json
$ slack-backup channel --name general > general.json
```

## Requirements

- slacko
- cmdliner
