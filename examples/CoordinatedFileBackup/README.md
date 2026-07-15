# Coordinated file backup

This self-contained example models a backup to a rate-limited remote target.
It creates demo files, submits one observable task per file to a bounded pool,
and groups the handles in a batch.

The required settings file is intentionally missing. The coordinator observes
that task's failure and applies an all-or-nothing policy: work that is still
pending is cancelled, while copies already running finish safely. It then
waits for the pool before releasing the object used by the callbacks.

The exact completed/cancelled counts may vary with scheduling. The important
invariants are that one file fails, pending tasks do not start after successful
cancellation, and running tasks are never interrupted.

Build with Lazarus or run:

```text
lazbuild CoordinatedFileBackup.lpi
```

The generated source and target directories are removed after the report.
