package main

import (
	"fmt"
	"os"
	"os/exec"
)

func main() {
	correctPrerequisite(detectDocker, "Could not detect docker")
}

func correctPrerequisite(detector func() error, msg string) {
	err := detector()
	if err != nil {
		fmt.Println(msg)
		os.Exit(1)
	}
}

func detectDocker() error {
	return detectCommand("docker")
}

func detectCommand(name string) error {
	cmd := exec.Command(name)
	_, err := cmd.CombinedOutput()
	if err != nil {
		return err
	}
	return nil
}
