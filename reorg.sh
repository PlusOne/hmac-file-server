#!/bin/bash
LOGFILE="reorg.log"

mkdir -p cmd/monitor cmd/server dashboard internal/config internal/storage test

mv main.go cmd/server/ 2>> $LOGFILE || echo "main.go not found" >> $LOGFILE
mv config.toml cmd/server/ 2>> $LOGFILE || echo "config.toml not found" >> $LOGFILE
mv hmac-file-server.log cmd/server/ 2>> $LOGFILE || echo "hmac-file-server.log not found" >> $LOGFILE
mv dashboard.json dashboard/ 2>> $LOGFILE || echo "dashboard.json not found" >> $LOGFILE
mv config.go internal/config/ 2>> $LOGFILE || echo "config.go not found" >> $LOGFILE
mv ftp.go iso.go local.go s3.go internal/storage/ 2>> $LOGFILE || echo "storage files not found" >> $LOGFILE
mv hmac_test.go hmac_icon.png test/ 2>> $LOGFILE || echo "test files not found" >> $LOGFILE

echo "Reorganization complete. Check $LOGFILE for details."
