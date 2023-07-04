package com.yahoo.container.handler;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.time.Instant;
import java.util.Optional;
import java.util.regex.Pattern;

public class ManualTest {

    @Test
    void test() {
        LogReader logReader = new LogReader(Path.of("/tmp/factory-logs/h200070a/logs/vespa"), Pattern.compile("vespa.log.*"));
        Instant from = Instant.parse("2023-06-23T00:00:00.00Z");
        Instant to = Instant.parse("2023-06-24T00:00:00.00Z");
        Optional<String> hostname = Optional.empty();
        logReader.writeLogs(System.out, from, to, 100_000L, hostname);
    }
}
