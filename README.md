## Files web server

This is a very simple HTTP server for sharing files and directories via web interface.

I host this on my Raspberry Pi for accessing files in home network. 
Just because I don't want to setup samba access for each device and sometimes need a way to share file. I can simply send a link to any device and download.

I'm not sure that this server is secure or can withstand attacks. Use it in private networks only.

I'm using Scala 3.2, builtin java HttpServer and [night-config](https://github.com/TheElectronWill/night-config) for config file.

## How to build and use

1. Make a jar file: ```sbt assembly```. It will appear inside directory ```target/scala-3.2.0```.
2. Create ```config.toml``` file and specify port and paths. See ```config example.conf```.
3. run ```java -jar fileswebserver-assembly-0.1.0-SNAPSHOT.jar```.
4. Access it in browser via ```127.0.0.1:8081/some/path```.

### config properties

* ```port```
* ```maxThreadsCount``` - maximum amount of simultaneous connections. Server spawn threads lazily, unused threads will be freed after 60 seconds of waiting.
* section for each entry point:
  * ```browserPath = "/some/other/Path"``` path in browser, for example ```127.0.0.1:8081/some/other/Path```
  * ```failIfMappedDirectoryNotExists``` fail fast if mapped directory doesn't exist in file system. If disabled, server will respond "not found" for mappings without file system directory. 
  * ```fsPath = "/file/system/other/path"``` corresponding directory in file system


