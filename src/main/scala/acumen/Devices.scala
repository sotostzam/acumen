package acumen


object Devices {

  def getDeviceInput[A](sourceId: Int, inputId: String): Value[A] = {
    val sourceData = updateDeviceData(sourceId)
    inputId match {
      case "ax" => VLit(GDouble(sourceData(0).toDouble))
      case "ay" => VLit(GDouble(sourceData(1).toDouble))
      case "az" => VLit(GDouble(sourceData(2).toDouble))
      case "alpha" => VLit(GDouble(sourceData(3).toDouble))
      case "beta" => VLit(GDouble(sourceData(4).toDouble))
      case "gamma" => VLit(GDouble(sourceData(5).toDouble))
      case "compassHeading" => VLit(GDouble(sourceData(6).toDouble))
      case _ => throw Errors.invalidInput(inputId)
    }
  }

  // get device data from server
  def updateDeviceData(deviceNo: Int): Array[String] = {
    val deviceData =
      //if (BuildHost.BuildHost.sensors.contains(deviceNo))
        BuildHost.BuildHost.sensors.get(deviceNo)
      //else throw Errors.invalidDevice(deviceNo)
    val dataAverage = deviceData.get(0)
    val deviceDataSize = deviceData.size
    var tempData = deviceData.get(0)
    var tempx = 0.0
    var tempy = 0.0
    var tempz = 0.0
    var tempalpha = 0.0
    var tempbeta = 0.0
    var tempgamma = 0.0
    var tempcompassheading = 0.0
    if (deviceDataSize > 1){
      for (i <- 0 until deviceDataSize){
        tempData = deviceData.get(i)
        tempx += tempData(0).toDouble
        tempy += tempData(1).toDouble
        tempz += tempData(2).toDouble
        tempalpha += tempData(3).toDouble
        tempbeta += tempData(4).toDouble
        tempgamma += tempData(5).toDouble
        tempcompassheading += tempData(6).toDouble
      }
      tempx /= deviceDataSize
      tempy /= deviceDataSize
      tempz /= deviceDataSize
      tempalpha /= deviceDataSize
      tempbeta /= deviceDataSize
      tempgamma /= deviceDataSize
      tempcompassheading /= deviceDataSize
      dataAverage(0) = tempx.toString
      dataAverage(1) = tempy.toString
      dataAverage(2) = tempz.toString
      dataAverage(3) = tempalpha.toString
      dataAverage(4) = tempbeta.toString
      dataAverage(5) = tempgamma.toString
      dataAverage(6) = tempcompassheading.toString
      deviceData.clear()
      deviceData.add(0, dataAverage)
      BuildHost.BuildHost.sensors.set(deviceNo, deviceData)
    }
    dataAverage
  }
}
