package main

import (
	"fmt"
	"mime"
)

func main() {
	fmt.Println("🔍 MIME Type Detection Test:")
	fmt.Println("JPG:", mime.TypeByExtension(".jpg"))
	fmt.Println("JPEG:", mime.TypeByExtension(".jpeg"))
	fmt.Println("PNG:", mime.TypeByExtension(".png"))
	fmt.Println("PDF:", mime.TypeByExtension(".pdf"))
	fmt.Println("TXT:", mime.TypeByExtension(".txt"))
	fmt.Println("ZIP:", mime.TypeByExtension(".zip"))
	fmt.Println("MP4:", mime.TypeByExtension(".mp4"))
	fmt.Println("HTML:", mime.TypeByExtension(".html"))
	fmt.Println("CSS:", mime.TypeByExtension(".css"))
	fmt.Println("JS:", mime.TypeByExtension(".js"))
	fmt.Println("Unknown:", mime.TypeByExtension(".xyz"))
	fmt.Println("Empty:", mime.TypeByExtension(""))
}
