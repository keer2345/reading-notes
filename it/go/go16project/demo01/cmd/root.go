package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var rootCmd = &cobra.Command{
	Use:   "root",
	Short: "short desc",
	Long:  "long description",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("root cmd run begin")

		// print flag
		fmt.Println(
			cmd.Flags().Lookup("viper").Value,
			cmd.PersistentFlags().Lookup("author").Value,
			cmd.PersistentFlags().Lookup("config").Value,
			cmd.PersistentFlags().Lookup("license").Value,
			cmd.Flags().Lookup("source").Value,
		)

		fmt.Println("---------------------------------")
		fmt.Println(
			viper.GetString("author"),
			viper.GetString("license"),
		)

		fmt.Println("root cmd run end")
	},
	TraverseChildren: true,
}

func Execute() {
	rootCmd.Execute()
}

var cfgFile string
var userLicense string

func init() {
	cobra.OnInitialize(initConfig)
	// 按名称接受命令行参数
	rootCmd.PersistentFlags().Bool("viper", true, "")
	// 指定 flag 缩写
	rootCmd.PersistentFlags().StringP("author", "a", "Your name", "")
	// 通过指针，将值赋值到字段
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "")
	// 通过指针，将值赋值到字段，并指定 flag 缩写
	rootCmd.PersistentFlags().StringVarP(&userLicense, "license", "l", "", "")
	// 添加本地标志
	rootCmd.Flags().StringP("source", "s", "", "")

	viper.BindPFlag("author", rootCmd.PersistentFlags().Lookup("author"))
	viper.BindPFlag("license", rootCmd.PersistentFlags().Lookup("license"))
	viper.SetDefault("author", "default author")
	viper.SetDefault("license", "default license")
}

func initConfig() {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	} else {
		home, err := os.UserHomeDir()
		cobra.CheckErr(err)
		viper.AddConfigPath(home)
		viper.SetConfigType("yaml")
		viper.SetConfigName(".cobra")
	}
	// 检查环境变量，将配置的键值加载到 viper
	viper.AutomaticEnv()

	if err := viper.ReadInConfig(); err != nil {
		fmt.Println(err)
	}

	fmt.Println("Using config file : ", viper.ConfigFileUsed())
}
