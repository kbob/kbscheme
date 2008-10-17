/* function whose return type is unconstructable? */

class C {
public:
    C();
    C(const C&);
    void operator=(const C&);
};

C f()
{
    return f();
}

int main()
{
}
