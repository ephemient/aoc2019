plugins {
    application
    id("com.github.ben-manes.versions")
    id("com.github.johnrengelman.shadow")
    id("io.gitlab.arturbosch.detekt")
    id("me.champeau.gradle.jmh")
    id("org.jetbrains.dokka")
    id("org.jmailen.kotlinter")
    kotlin("jvm")
    `maven-publish`
}
repositories {
    jcenter()
}

application {
    mainClassName = "io.github.ephemient.aoc2019.MainKt"
    //applicationDefaultJvmArgs = listOf("-Xmx3072m")
}

val jar by tasks.getting(Jar::class) {
    manifest {
        attributes["Main-Class"] = "io.github.ephemient.aoc2019.MainKt"
    }
}
 
defaultTasks = listOf("check", "run")

dependencies {
    val guavaVersion: String by project
    val junitVersion: String by project
    val kotlinxCoroutinesVersion: String by project
    val truthVersion: String by project

    implementation(kotlin("stdlib-jdk8"))
    implementation("com.google.guava:guava:$guavaVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:$kotlinxCoroutinesVersion")
    testImplementation(kotlin("test-junit5"))
    testImplementation("com.google.truth:truth:$truthVersion")
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:$kotlinxCoroutinesVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-api:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-params:$junitVersion")
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

detekt {
    val detektVersion: String by project
    toolVersion = detektVersion
    config = rootProject.files("detekt.yml")
    buildUponDefaultConfig = true
    baseline = rootProject.file("detekt-baseline.xml")
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
    args("applyToIDEAProject", "-y")
}

tasks.wrapper {
    gradleVersion = "6.0.1"
}
