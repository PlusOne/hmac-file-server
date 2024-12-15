package config

type ServerConfig struct {
    ListenPort     string `mapstructure:"ListenPort"`
    StoragePath    string `mapstructure:"StoragePath"`
    FileTTL        string `mapstructure:"FileTTL"`
    MetricsEnabled bool   `mapstructure:"MetricsEnabled"`
    MetricsPort    string `mapstructure:"MetricsPort"`
    UnixSocket     bool   `mapstructure:"UnixSocket"`
}

type ISOConfig struct {
    Enabled bool `mapstructure:"Enabled"`
}

type TimeoutConfig struct {
    ReadTimeout  string `mapstructure:"ReadTimeout"`
    WriteTimeout string `mapstructure:"WriteTimeout"`
    IdleTimeout  string `mapstructure:"IdleTimeout"`
}

type WorkersConfig struct {
    UploadQueueSize int `mapstructure:"UploadQueueSize"`
}

type ClamAVConfig struct {
    ClamAVEnabled bool   `mapstructure:"ClamAVEnabled"`
    ClamAVSocket  string `mapstructure:"ClamAVSocket"`
}

type RedisConfig struct {
    RedisEnabled             bool   `mapstructure:"RedisEnabled"`
    RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

type Config struct {
    Server   ServerConfig   `mapstructure:"server"`
    ISO      ISOConfig      `mapstructure:"iso"`
    Timeouts TimeoutConfig  `mapstructure:"timeouts"`
    Workers  WorkersConfig  `mapstructure:"workers"`
    ClamAV   ClamAVConfig   `mapstructure:"clamav"`
    Redis    RedisConfig    `mapstructure:"redis"`
}

func LoadConfig(configFile string) (*Config, error) {
    var conf Config
    viper.SetConfigFile(configFile)
    if err := viper.ReadInConfig(); err != nil {
        return nil, err
    }
    if err := viper.Unmarshal(&conf); err != nil {
        return nil, err
    }
    return &conf, nil
}
