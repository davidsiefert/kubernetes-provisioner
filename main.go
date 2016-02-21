package main

import (
	"fmt"
	"github.com/c4milo/unzipit"
	"io"
	"net/http"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
)

func main() {
	err := provisionEtcd()
	if err != nil {
		fmt.Printf("Failed to provision etcd: %s\n", err)
		os.Exit(1)
	}

	err = provisionKubernetes()
	if err != nil {
		fmt.Printf("Failed to provision kubernetes: %s\n", err)
		os.Exit(1)
	}

	correctPrerequisite(detectDocker, "Could not detect docker")
}

func provisionEtcd() error {
	path, err := downloadEtcd()
	if err != nil {
		return err
	}

	extractErr := extract(path, userHome())
	if extractErr != nil {
		return extractErr
	}

	return nil
}

func provisionKubernetes() error {
	path, err := downloadKubernetes()
	if err != nil {
		return err
	}

	extractErr := extract(path, userHome())
	if extractErr != nil {
		return extractErr
	}

	return nil
}

func extract(srcArchive, destDir string) error {
	archive, openErr := os.Open(srcArchive)
	if openErr != nil {
		return openErr
	}
	defer archive.Close()

	_, unpackErr := unzipit.Unpack(archive, destDir)
	if unpackErr != nil {
		return unpackErr
	}

	return nil
}

func userHome() string {
	usr, err := user.Current()
	if err != nil {
		panic(err)
	}
	return usr.HomeDir
}

func downloadEtcd() (string, error) {
	return download("https://github.com/coreos/etcd/releases/download/v2.0.12/etcd-v2.0.12-linux-amd64.tar.gz", "etcd-2.0.12.tar.gz")
}

func downloadKubernetes() (string, error) {
	return download("https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz", "kubernetes-1.1.2.tar.gz")
}

func download(url, destName string) (string, error) {
	downloadsDir := filepath.Join(userHome(), ".downloads")
	err := os.MkdirAll(downloadsDir, 0777)
	if err != nil {
		return "", err
	}

	targetFilePath := filepath.Join(downloadsDir, destName)
	if _, err := os.Stat(targetFilePath); os.IsNotExist(err) {
		targetFile, targetFileErr := os.Create(targetFilePath)
		if targetFileErr != nil {
			return "", targetFileErr
		}
		defer targetFile.Close()

		response, httpErr := http.Get(url)
		if httpErr != nil {
			return "", httpErr
		}
		defer response.Body.Close()

		_, writeErr := io.Copy(targetFile, response.Body)
		if writeErr != nil {
			return "", writeErr
		}
	}

	return targetFilePath, nil
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
