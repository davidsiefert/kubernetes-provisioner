package main

import (
	"fmt"
	"os"
	"os/user"
	"os/exec"
	"path/filepath"
	"io"
	"io/ioutil"
	"net/http"
	"github.com/c4milo/unzipit"
)

func main() {
	path, err := downloadKubernetes()
	if err != nil {
		fmt.Printf("Failed to download kubernetes: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("Kubernetes downloaded to %s\n", path)

	extractErr := extract(path, userHome())
	if extractErr != nil {
		fmt.Printf("Failed to extract kubernetes: %s\n", extractErr)
		os.Exit(1)
	}

	correctPrerequisite(detectDocker, "Could not detect docker")
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
	
func downloadKubernetes() (string, error) {
	tempDirPath, tempDirErr := ioutil.TempDir(os.TempDir(), "kubernetes")
	if tempDirErr != nil {
		return "", tempDirErr
	}

	kubernetesFilePath := filepath.Join(tempDirPath, "kubernetes-1.1.2.tar.gz")
	kubernetesFile, kubernetesFileErr := os.Create(kubernetesFilePath)
	if kubernetesFileErr != nil {
		return "", kubernetesFileErr
	}
	defer kubernetesFile.Close()

	response, httpErr := http.Get("https://github.com/kubernetes/kubernetes/releases/download/v1.1.2/kubernetes.tar.gz")
	if httpErr != nil {
		return "", httpErr
	}
	defer response.Body.Close()

	_, writeErr := io.Copy(kubernetesFile, response.Body)
	if writeErr != nil {
		return "", writeErr
	}

	return kubernetesFilePath, nil
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
