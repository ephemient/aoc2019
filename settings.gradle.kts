pluginManagement {
    plugins {
        val detektVersion: String by settings
        val dokkaVersion: String by settings
        val jmhVersion: String by settings
        val kotlinVersion: String by settings
        val kotlinterVersion: String by settings
        val shadowVersion: String by settings
        val versionsPluginVersion: String by settings

        id("com.github.ben-manes.versions") version versionsPluginVersion
        id("com.github.johnrengelman.shadow") version shadowVersion
        id("io.gitlab.arturbosch.detekt") version detektVersion
        id("me.champeau.gradle.jmh") version jmhVersion
        id("org.jetbrains.dokka") version dokkaVersion
        id("org.jmailen.kotlinter") version kotlinterVersion
        kotlin("jvm") version kotlinVersion
    }
}
rootProject.name = "aoc2019"
