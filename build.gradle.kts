plugins {
    id("org.jetbrains.dokka")
    kotlin("jvm")
    id("org.jmailen.kotlinter")
    id("me.champeau.gradle.jmh")
    id("com.github.ben-manes.versions")
    application
    `maven-publish`
}
repositories {
    jcenter()
}

application {
    mainClassName = "io.github.ephemient.aoc2019.MainKt"
    //applicationDefaultJvmArgs = listOf("-Xmx3072m")
}
 
defaultTasks = listOf("check", "run")

dependencies {
    val junitVersion: String by project

    implementation(kotlin("reflect"))
    implementation(kotlin("stdlib-jdk8"))
    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
    jmhImplementation(kotlin("reflect"))
    jmhImplementation(kotlin("stdlib-jdk8"))
}

tasks.withType<JavaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
    options.encoding = "UTF-8"
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

kotlinter {
    ignoreFailures = project.hasProperty("lintContinueOnError")
    experimentalRules = project.hasProperty("lintKotlinExperimental")
}

tasks.withType<Test> {
    useJUnitPlatform()
    testLogging.showStandardStreams = true
}

val jmhExclude: String? by project
val jmhInclude: String? by project

jmh {
    benchmarkMode = listOf("sample")
    if (!jmhExclude.isNullOrEmpty()) exclude = listOf(jmhExclude)
    if (!jmhInclude.isNullOrEmpty()) include = listOf(jmhInclude)
    fork = 1
    threads = 1
    timeOnIteration = "1s"
    timeUnit = "ms"
    warmupIterations = 1
}

val dokka by tasks.getting(org.jetbrains.dokka.gradle.DokkaTask::class) {
    configuration {
        includes = listOf("README.md")
        jdkVersion = 8
        sourceLink {
            path = "src/main/kotlin"
            url = "https://github.com/ephemient/aoc2018/blob/kotlin/src/main/kotlin"
            lineSuffix = "#L"
        }
    }
}

publishing {
    publications {
        create<MavenPublication>(project.name) {
            val dokkaJar by tasks.creating(Jar::class) {
                from(dokka)
                classifier = "javadoc"
            }
            artifact(dokkaJar)
        }
    }
}

val ktlintIdea by tasks.creating(JavaExec::class) {
    group = "IDE"
    description = "Apply ktlint style to IntelliJ IDEA project."
    main = "com.pinterest.ktlint.Main"
    classpath = buildscript.configurations["classpath"]
    args("--apply-to-idea", "-y")
}

tasks.wrapper {
    gradleVersion = "6.0"
}
