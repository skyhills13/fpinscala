package fpinscala.laziness.toby;

import java.util.function.UnaryOperator;
import java.util.stream.*;

/**
 * Created by toby on 21/02/2017.
 */
public class Streamss {
    public static void main(String[] args) {
        java.util.stream.Stream<Integer> ones = java.util.stream.Stream.<Integer>iterate(1, x -> x);
        System.out.println(ones.map(a -> a + 1).limit(5).collect(Collectors.toList()));
    }
}
