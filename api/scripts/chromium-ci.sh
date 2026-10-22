#!/bin/sh
set -eu
# Chromium's histogram shared-memory launch path can trigger an FD ownership
# crash in constrained Linux containers. Keep the browser multi-process and
# disable only that feature for the CI acceptance browser.
exec /usr/bin/chromium --disable-features=PassHistogramSharedMemoryOnLaunch "$@"
