import it.unimi.dsi.fastutil.ints.Int2ObjectArrayMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("unused")
public final class FileSystem {

    public static final FileSystem $INSTANCE = new FileSystem();

    private final AtomicInteger idGen = new AtomicInteger();
    private final Int2ObjectMap<FileReader> readers = new Int2ObjectArrayMap<>();
    private final Int2ObjectArrayMap<FileWriter> writers = new Int2ObjectArrayMap<>();

    public int openR(String path) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var id = idGen.incrementAndGet();
        var reader = new FileReader(path);
        readers.put(id, reader);
        return id;
    }

    public int openW(String path) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var id = idGen.incrementAndGet();
        var writer = new FileWriter(path);
        writers.put(id, writer);
        return id;
    }

    public int openA(String path) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var id = idGen.incrementAndGet();
        var writer = new FileWriter(path, true);
        writers.put(id, writer);
        return id;
    }

    public void write(int fileId, String s) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var writer = writers.get(fileId);
        if (writer == null) {
            throw new IllegalArgumentException("no file with the given id is currently open write mode");
        }
        writer.write(s);
    }

    public int read(int fileId) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var reader = readers.get(fileId);
        if (reader == null) {
            throw new IllegalArgumentException("no file with the given id is currently open write mode");
        }
        return reader.read();
    }

    public void close(int fileId) throws IOException {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var reader = readers.remove(fileId);
        if (reader != null) {
            reader.close();
        }
        var writer = writers.remove(fileId);
        if (writer != null) {
            writer.close();
        }
    }

    public boolean createDir(String path) {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var dir = new File(path);
        var alreadyExists = dir.exists();
        if (alreadyExists) {
            return true;
        }
        return dir.mkdirs();
    }

    public boolean delete(String path) {
        Rattlesnake$runtime.assertFileSystemAllowed();
        var file = new File(path);
        return file.delete();
    }

}

