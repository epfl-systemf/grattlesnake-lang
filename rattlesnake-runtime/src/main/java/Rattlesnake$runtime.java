import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;

import java.util.WeakHashMap;

@SuppressWarnings("unused")
public final class Rattlesnake$runtime {

    private Rattlesnake$runtime() {
    }

    private static final WeakHashMap<Object, Integer> objToRegion = new WeakHashMap<>();

    public static void saveObjectInRegion(Object obj, int region) {
        objToRegion.put(obj, region);
    }

    public static int regionOf(Object obj){
        return objToRegion.get(obj);
    }

    // NOTE: This implementation assumes single-threaded execution.
    // A thread-safe runtime should include per-thread capability stacks.

    private static DynamicEnvironment currentEnvir = null;
    private static DynamicEnvironment inPrepEnvir = null;

    private static final class DynamicEnvironment {
        private boolean fileSystem = false;
        private boolean console = false;
        private final IntSet regions = new IntOpenHashSet();
        private DynamicEnvironment next = null;
    }

    public static void startPreparingEnvir() {
        inPrepEnvir = new DynamicEnvironment();
    }

    public static void allowConsole(){
        assertConsoleAllowed();
        inPrepEnvir.console = true;
    }

    public static void allowFilesystem() {
        assertFileSystemAllowed();
        inPrepEnvir.fileSystem = true;
    }

    public static void allowRegion(int region) {
        assertRegionAllowed(region);
        inPrepEnvir.regions.add(region);
    }

    public static void pushEnvir() {
        inPrepEnvir.next = currentEnvir;
        currentEnvir = inPrepEnvir;
        inPrepEnvir = null;
    }

    public static void popEnvir() {
        currentEnvir = currentEnvir.next;
    }

    public static void assertConsoleAllowed() {
        if (currentEnvir != null && !currentEnvir.console){
            throw new Rattlesnake$IllegalCapabilityUseError(
                    "illegal use of the console in a dynamically restricted environment");
        }
    }

    public static void assertFileSystemAllowed() {
        if (currentEnvir != null && !currentEnvir.fileSystem) {
            throw new Rattlesnake$IllegalCapabilityUseError(
                    "illegal use of the file system in a dynamically restricted environment");
        }
    }

    /**
     * This method assumes that currentEnvir is not null
     */
    private static void assertRegionOfIdIsAllowed(int regionId) {
        if (!currentEnvir.regions.contains(regionId)) {
            throw new Rattlesnake$IllegalCapabilityUseError(
                    "illegal modification of a region in a dynamically restricted environment");
        }
    }

    public static void assertRegionAllowed(Object obj) {
        if (currentEnvir == null){
            return;
        }
        var region = objToRegion.get(obj);
        if (region != null) {
            assertRegionOfIdIsAllowed(region);
        }
    }

    public static void monitoredArrayStore(int[] array, int idx, int value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(long[] array, int idx, long value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(float[] array, int idx, float value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(double[] array, int idx, double value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(Object[] array, int idx, Object value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(boolean[] array, int idx, boolean value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(char[] array, int idx, char value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(short[] array, int idx, short value) {
        assertRegionAllowed(array);
        array[idx] = value;
    }

    /// / REGIONS ////

    private static int lastRegion = 0;

    public static int newRegion() {
        var r = ++lastRegion;
        if (currentEnvir != null) {
            currentEnvir.regions.add(r);
        }
        return r;
    }

}
