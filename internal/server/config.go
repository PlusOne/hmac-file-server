type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytes         int64  `mapstructure:"MinFreeBytes"` // Minimum free bytes required
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled"`
}
type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}
type SecurityConfig struct {
	Secret string `mapstructure:"Secret"`
}
type VersioningConfig struct {
	EnableVersioning bool `mapstructure:"EnableVersioning"`
	MaxVersions      int  `mapstructure:"MaxVersions"`
}
type UploadsConfig struct {
	ResumableUploadsEnabled bool     `mapstructure:"ResumableUploadsEnabled"`
	ChunkedUploadsEnabled   bool     `mapstructure:"ChunkedUploadsEnabled"`
	ChunkSize               int64    `mapstructure:"ChunkSize"`
	AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
}
type ClamAVConfig struct {
	ClamAVEnabled  bool   `mapstructure:"ClamAVEnabled"`
	ClamAVSocket   string `mapstructure:"ClamAVSocket"`
	NumScanWorkers int    `mapstructure:"NumScanWorkers"`
}
type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"RedisEnabled"`
	RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
	RedisAddr                string `mapstructure:"RedisAddr"`
	RedisPassword            string `mapstructure:"RedisPassword"`
	RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}
type WorkersConfig struct {
	NumWorkers      int `mapstructure:"NumWorkers"`
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
}
type FileConfig struct {
	FileRevision int `mapstructure:"FileRevision"`
}
type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"` // Add this line
}
type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Security   SecurityConfig   `mapstructure:"security"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	File       FileConfig       `mapstructure:"file"`
	ISO        ISOConfig        `mapstructure:"iso"`
}
func readConfig(configFilename string, conf *Config) error {
	viper.SetConfigFile(configFilename)
	viper.SetConfigType("toml")

	// Read in environment variables that match
	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC") // Prefix for environment variables

	// Read the config file
	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}
func setDefaults() {
	// Server defaults
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "8760h")      // 365d -> 8760h
	viper.SetDefault("server.MinFreeBytes", 100<<20) // 100 MB

	// Timeout defaults
	viper.SetDefault("timeouts.ReadTimeout", "4800s") // supports 's'
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "4800s")

	// Security defaults
	viper.SetDefault("security.Secret", "changeme")

	// Versioning defaults
	viper.SetDefault("versioning.EnableVersioning", false)
	viper.SetDefault("versioning.MaxVersions", 1)

	// Uploads defaults
	viper.SetDefault("uploads.ResumableUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkedUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkSize", 8192)
	viper.SetDefault("uploads.AllowedExtensions", []string{
		".txt", ".pdf",
		".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2",
		".mp3", ".ogg",
	})
func validateConfig(conf *Config) error {
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("ListenPort must be set")
	}
