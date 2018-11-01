package com.github.johnynek.bazel_deps

import java.io.File
import org.apache.maven.settings.Settings
import org.apache.maven.settings.building.{DefaultSettingsBuilderFactory, DefaultSettingsBuildingRequest}
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._

object SettingsLoader {
  private[this] val logger = LoggerFactory.getLogger(getClass)
  private val m2Home = new File(new File(System.getProperty("user.home")), "/.m2/")

  lazy val settings: Settings = {

    val settingsResult = new DefaultSettingsBuilderFactory()
      .newInstance()
      .build(new DefaultSettingsBuildingRequest()
        .setUserSettingsFile(new File(m2Home, "settings.xml"))
      )

    settingsResult.getProblems.foreach{ problem =>
      logger.warn(problem.toString)
    }

    settingsResult.getEffectiveSettings
  }
}
