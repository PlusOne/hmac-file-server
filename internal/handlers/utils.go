// internal/utils/utils.go
package metrics

import (
    "log"
)

func InitializeLogging(level string) {
    // Setup logging based on level
    log.Println("Logging initialized with level:", level)
}