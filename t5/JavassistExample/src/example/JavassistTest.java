/**
 * @author Pedro Probst
 */

package example;

import static javassist.ClassPool.getDefault;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javassist.*;

public class JavassistTest {

    public static void main (String[] args) {
        alteraHelloWorld();
        System.out.println();

        // class Point()
        criaClasse("example.Point");
        criaAtributo("example.Point", new String[]{"x", "y"});
        criaMetodo("example.Point", "public void move(int dx, int dy) { x += dx; y += dy; }");
        criaMetodo("example.Point", "public void print() { System.out.println(\"x: \" + x + \" y: \" + y); }");

        // Testando pt
        try {
            Class point = getDefault().get("example.Point").toClass(); // dá warning no Java 11+.. ignorar.
            Method print = point.getDeclaredMethod("print");
            Method move = point.getDeclaredMethod("move", int.class, int.class);
            System.out.println("> New instance of Point");
            Object pt = point.getConstructor().newInstance();
            System.out.print("> Início: ");
            print.invoke(pt);
            System.out.println("> Executando move(10, 20): ");
            move.invoke(pt, 10, 20);
            System.out.print("> Agora: ");
            print.invoke(pt);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | NotFoundException | CannotCompileException | InstantiationException e) {
            e.printStackTrace();
        }
    }

    // Exemplo mais simples do uso do Javassist: modificar o corpo de método de uma classe já existente:
    // (introspecção e customização)
    public static void alteraHelloWorld() {
        try {
            // 1) Pega a classe criada.
            CtClass ctClass = getDefault().get("example.HelloWorld");
            // 2) Da classe criada, pegue um método.
            CtMethod ctMethod = ctClass.getDeclaredMethod(("sayHello"));
            // 3) Crie um corpo para o método.
            ctMethod.setBody("{ System.out.println(\"HelloWord.sayHello(): Corpo alterado do metodo sayHello()!\"); }");
            // 4) Salve. Dependendo da IDE usada, altere o caminho para onde estão os arquivos .class!
            ctClass.writeFile("out/production/JavassistExample"); // caminho no IntelliJ IDEA
            // Teste.
            HelloWorld hw = new HelloWorld();
            hw.sayHello();
        } catch (NotFoundException | CannotCompileException | IOException e) {
            e.printStackTrace();
        }
    }

    //  Criando uma classe programaticamente.
    public static void criaClasse(String className) {
        System.out.println("> Criando a classe:\n -- " + className + "\n");
        try {
            ClassPool cp = ClassPool.getDefault();
            CtClass cc = cp.makeClass(className);
            cc.writeFile("out/production/JavassistExample");
            cc.defrost();
            //return cc.toClass();
        } catch (CannotCompileException | IOException e) {
            e.printStackTrace();
        }
    }

    // Criando atributos programaticamente.
    public static void criaAtributo(String className, String[] attributes) {
        System.out.println("> Criando os atributos:");
        try {
            CtClass cc = ClassPool.getDefault().get(className);
            for (int i = 0; i < attributes.length; i++) {
                CtField cf = new CtField(CtClass.intType, attributes[i], cc);
                System.out.println(" -- " + attributes[i]);
                cc.addField(cf, "0");
            }
            System.out.println("Para a classe " + className + "\n");
            cc.writeFile();
            cc.defrost();
        } catch (CannotCompileException | IOException | NotFoundException e) {
            e.printStackTrace();
        }
    }

    // Criando um método programaticamente.
    public static void criaMetodo(String className, String methodBody) {
        System.out.println("> Criando o método\n" + "-- " + methodBody + "\n" + "para a classe " + className + "\n");
        try {
            CtClass cc = ClassPool.getDefault().get(className);
            CtMethod m = CtNewMethod.make(methodBody, cc);
            cc.addMethod(m);
            cc.writeFile("out/production/JavassistExample");
            cc.defrost();
        } catch (CannotCompileException | IOException | NotFoundException e) {
            e.printStackTrace();
        }
    }
}
