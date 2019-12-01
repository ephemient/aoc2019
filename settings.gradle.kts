pluginManagement {
    plugins {
        val detektVersion: String by settings
        val dokkaVersion: String by settings
        val kotlinVersion: String by settings
        val kotlinterVersion: String by settings
        val jmhVersion: String by settings
        val versionsPluginVersion: String by settings

        id("io.gitlab.arturbosch.detekt") version detektVersion
        id("org.jetbrains.dokka") version dokkaVersion
        kotlin("jvm") version kotlinVersion
        id("org.jmailen.kotlinter") version kotlinterVersion
        id("me.champeau.gradle.jmh") version jmhVersion
        id("com.github.ben-manes.versions") version versionsPluginVersion
    }
}
rootProject.name = "aoc2019"
