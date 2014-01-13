Abstract syntax tree for `((lambda (a) (a b)) c)`:

    app-exp:
        rator:
            lambda-exp:
                bound-var: a
                body:
                    app-exp:
                        rator:
                            var-exp:
                                var: a
                        rand:
                            var-exp:
                                var: b
        rand:
            var-exp:
                var: c
        
Abstract syntax tree for `(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))`


    lambda-exp:
        bound-var: x
        body:
            lambda-exp:
                bound-var: y
                body:
                    app-exp:
                        rator:
                            lambda-exp:
                                bound-var: x
                                body:
                                    app-exp:
                                        rator:
                                            var-exp:
                                                var: x
                                        rand:
                                            var-exp:
                                                var: y
                        rand:
                            var-exp:
                                var: x
