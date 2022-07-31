
structure AMD64UnixCMB : CMB =
    BootstrapCompileFn (structure Backend = AMD64CCallBackend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)


