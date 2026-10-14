package top.kzre.homunculus.lang;

public final class Var implements IFn, IRef{
    public final Symbol sym;
    public final Namespace ns;
    volatile Object root;

    public Var(Symbol sym, Namespace ns, Object root){
        this.sym = sym;
        this.ns = ns;
        this.root = root;
    }

    @Override
    public Object invoke(Object... params) {
        return this.fn().invoke(params);
    }

    @Override
    public final Object deref() {
        return this.root;
    }

    public final IFn fn(){
        if (this.root instanceof IFn){
            return (IFn)this.root;
        }else {
            throw new IllegalStateException(
                    "Var " + ns.getName() + "/" + sym.getName() + " is not a function: " + root);
        }
    }

    public final Object getRawRoot() {
        return this.root;
    }
}
