package main

import (
	"fmt"
	"os"
	"path"
	"testing"
)

var binaryName string

func TestMain(t *testing.M) {
}
func TestApplication(t *testing.T) {
	curDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	binaryPath := path.Join(curDir, binaryName)

	tests := []struct {
		args                []string
		input               string
		expectedOutputLines []string
		expectedExitCode    int
	}{
		{
			args:             []string{},
			expectedExitCode: 1,
			expectedOutputLines: []string{

				"invalid number of arguments",
				fmt.Sprintf("Usage: %s <integer> [-h|-help]", binaryPath),
				"",
				"A greeter application which prints the name you entered <integer> number of times.",
				"",
			},
		},
		{
			args:             []string{"-h"},
			expectedExitCode: 1,
			expectedOutputLines: []string{
				"Must specify a number greater than 0",
				fmt.Sprintf("Usage: %s <integer> [-h|-help]", binaryPath),
				"",
				"A greeter application which prints the name you entered <integer> number of times.",
				"",
			},
		},
		{
			args:                []string{"a"},
			expectedExitCode:    1,
			expectedOutputLines: []string{},
		},
		{
			args:             []string{"2"},
			input:            "jane doe",
			expectedExitCode: 0,
			expectedOutputLines: []string{
				"Your name please? Press the Enter key when done.",
				"Nice to meet you jane doe",
			},
		},
	}
}
