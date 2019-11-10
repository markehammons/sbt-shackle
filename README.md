# SBT-shackle

An sbt plugin for generating scala bindings to C libraries

#### Limitations

* Shackle cannot bind to c methods with varargs
* Only outputs dotty compatible scala code
* Requires panama early access build 14-panama+1-15
* Methods, parameters, and struct members may be renamed if they are scala keywords or otherwise not workable as scala identifiers. Usually, this rename takes the form of adding an $ at the end (such as `class$` instead of `class`).

## Settings

* headers: Header files to pass to jextract
* includePaths: Paths to include directories that jextraxt will need
* libraryPaths: Paths to libraries that jextract will link to
* clangOptions: Options for jextract to pass to clang
* library: Library to bind to
* libraryPackage: The java package the library bindings will be put in
* packageMappings: Mappings of include folders to java packages

## Tasks

* jextract: Runs the jextract command and generates java bindings for the C library
* shackle: Parses the java bindings and generates scala (dotty) bindings for the C library

### Example Settings for binding to wlroots
```scala
//we're binding to wlroots
library := "wlroots"

//we want the wlroots bindings to be in the wlroots package
libraryPackage := "wlroots"

//my system's include directory
val includeDirectory = file("/usr/include")

//headers from wlroots that we specifically want to bind to
headers := Set(
  includeDirectory / "wlr/types/wlr_output.h",
  includeDirectory / "wlr/backend.h",
  includeDirectory / "wlr/render/wlr_renderer.h",
  includeDirectory / "wlr/types/wlr_idle.h",
  includeDirectory / "wlr/types/wlr_gamma_control_v1.h",
  includeDirectory / "wlr/types/wlr_screencopy_v1.h",
  includeDirectory / "wlr/types/wlr_compositor.h",
  includeDirectory / "wlr/types/wlr_primary_selection_v1.h",
  includeDirectory / "wlr/types/wlr_xdg_shell_v6.h",
  includeDirectory / "wlr/types/wlr_surface.h",
  includeDirectory / "wlr/types/wlr_box.h",
  includeDirectory / "wlr/types/wlr_matrix.h"
)

//resolve naming errors and conflicts in jextract by manually changing the package names corresponding to certain folders
packageMappings := Map(
  file("/usr/include/wlr/backend") -> "wlroots.backend_headers",
  file("/usr/include/bits/types") -> "usr.include.bits.type_headers"
)

//wlroots needs a special flag set to work, so we set that here
clangOptions := Set("-DWLR_USE_UNSTABLE")

//paths to headers that might be needed for jextract to make bindings
includePaths := Set(
  includeDirectory / "wlr",
  includeDirectory / "wayland",
  includeDirectory / "pixman-1",
  includeDirectory / "libxkbcommon"
) + xdgGenDir.value


//where on your system jextract should search for .so files
libraryPaths := Set(
  file("/usr/lib64")
)

//my wlroots binding needs the xdgShellProtocol genererated, so these tasks and settings do that
lazy val xdgShellProtocolLocation = settingKey[File]("location of xdg-shell-unstable-v6.xml on your system")

lazy val xdgProtocolGen = taskKey[File]("generates the xdg-shell-protocol.h header")

lazy val xdgGenDir = settingKey[File]("where to create the xdg headers") 

xdgShellProtocolLocation := file("/usr/share/wayland-protocols/unstable/xdg-shell/xdg-shell-unstable-v6.xml")

xdgGenDir := baseDirectory.value / "include"


xdgProtocolGen := {
  val logger = streams.value
  import scala.sys.process.Process

  val includeDir = xdgGenDir.value

  includeDir.mkdir()

  val proc = Process("wayland-scanner", Seq("server-header", xdgShellProtocolLocation.value.getCanonicalPath, s"${includeDir.getCanonicalPath}/xdg-shell-unstable-v6-protocol.h")).run()
  val exitCode = proc.exitValue()

  if(exitCode != 0) {
    sys.error("failed to generate the xdg protocol")
  }

  xdgGenDir.value
}

//now I say that jextract is dependent on the generation of the xdgProtocolHeaders 
jextract := {
  xdgProtocolGen.value
  jextract.value
}
```
