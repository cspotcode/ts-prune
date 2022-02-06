/* eslint-disable no-inner-declarations */
import { ignoreComment } from "./constants";
import * as real_ts from 'typescript';
import {
  ExportDeclaration,
  ImportDeclaration,
  Project,
  SourceFile,
  SourceFileReferencingNodes,
  ts,
  Symbol,
  SyntaxKind,
  StringLiteral,
  ObjectBindingPattern,
} from "ts-morph";
import { isDefinitelyUsedImport } from "./util/isDefinitelyUsedImport";
import { getModuleSourceFile } from "./util/getModuleSourceFile";
import { getNodesOfKind } from './util/getNodesOfKind';
import countBy from "lodash/fp/countBy";
import last from "lodash/fp/last";
import { realpathSync } from "fs";
import { IConfigInterface } from "./configurator";
import { F } from "lodash/fp";

declare module "typescript" {
  namespace FindAllReferences.Core {
    function getReferencedSymbolsForSymbol(
      originalSymbol: Symbol, node: Node | undefined, sourceFiles: readonly SourceFile[], sourceFilesSet: ReadonlySet<string>, checker: TypeChecker, cancellationToken: CancellationToken, options: Options, __callback__: Function): SymbolAndEntries[];
  }
}

type OnResultType = (result: IAnalysedResult) => void;

export enum AnalysisResultTypeEnum {
  POTENTIALLY_UNUSED,
  DEFINITELY_USED
}

export type ResultSymbol = {
  name: string;
  line?: number;
  usedInModule: boolean;
};

export type IAnalysedResult = {
  file: string;
  type: AnalysisResultTypeEnum;
  symbols: ResultSymbol[];
}

function handleExportDeclaration(node: SourceFileReferencingNodes) {
  return (node as ExportDeclaration).getNamedExports().map(n => n.getName());
}

function handleImportDeclaration(node: ImportDeclaration) {
  return (
    [
      ...node.getNamedImports().map(n => n.getName()),
      ...(node.getDefaultImport() ? ['default'] : []),
      ...(node.getNamespaceImport() ? trackWildcardUses(node) : []),
    ]
  );
}

/**
 * Given an `import * as foo from './foo'` import, figure out which symbols in foo are used.
 *
 * If there are uses which cannot be tracked, this returns ["*"].
 */
export const trackWildcardUses = (node: ImportDeclaration) => {
  const clause = node.getImportClause();
  const namespaceImport = clause.getFirstChildByKind(ts.SyntaxKind.NamespaceImport);
  const source = node.getSourceFile();

  const uses = getNodesOfKind(source, ts.SyntaxKind.Identifier)
    .filter(n => (n.getSymbol()?.getDeclarations() ?? []).includes(namespaceImport));

  const symbols: string[] = [];
  for (const use of uses) {
    if (use.getParentIfKind(SyntaxKind.NamespaceImport)) {
      // This is the "import * as module" line.
      continue;
    }

    const p = use.getParentIfKind(SyntaxKind.PropertyAccessExpression);
    if (p) {
      // e.g. `module.x`
      symbols.push(p.getName());
      continue;
    }

    const el = use.getParentIfKind(SyntaxKind.ElementAccessExpression);
    if (el) {
      const arg = el.getArgumentExpression();
      if (arg.getKind() === SyntaxKind.StringLiteral) {
        // e.g. `module['x']`
        symbols.push((arg as StringLiteral).getLiteralText());
        continue;
      }
    }

    const varExp = use.getParentIfKind(SyntaxKind.VariableDeclaration);
    if (varExp) {
      const nameNode = varExp.getNameNode();
      if (nameNode.getKind() === SyntaxKind.ObjectBindingPattern) {
        const binder = (nameNode as ObjectBindingPattern);
        for (const bindEl of binder.getElements()) {
          const p = bindEl.getPropertyNameNode();
          if (p) {
            // e.g. const {z: {a}} = module;
            symbols.push(p.getText());
          } else {
            // e.g. const {x} = module;
            symbols.push(bindEl.getName());
          }
        }
        continue;
      }
    }

    const qualExp = use.getParentIfKind(SyntaxKind.QualifiedName);
    if (qualExp) {
      // e.g. type T = module.TypeName;
      symbols.push(qualExp.getRight().getText());
      continue;
    }

    // If we don't understand a use, be conservative.
    return ['*'];
  }

  return symbols;
};

// like import("../xyz")
function handleDynamicImport(node: SourceFileReferencingNodes) {
  // a dynamic import always imports all elements, so we can't tell if only some are used
  return ["*"];
}

const nodeHandlers = {
  [ts.SyntaxKind.ExportDeclaration.toString()]: handleExportDeclaration,
  [ts.SyntaxKind.ImportDeclaration.toString()]: handleImportDeclaration,
  [ts.SyntaxKind.CallExpression.toString()]: handleDynamicImport,
};

const mustIgnore = (symbol: Symbol, file: SourceFile) => {
  const symbolLinePos = symbol
    .getDeclarations()
    .map((decl) => decl.getStartLinePos())
    .reduce((currentMin, current) => Math.min(currentMin, current), Infinity);

  const comments = file
    .getDescendantAtPos(symbolLinePos)
    ?.getLeadingCommentRanges();

  if (!comments) {
    return false;
  }

  return last(comments)?.getText().includes(ignoreComment);
};

const lineNumber = (symbol: Symbol) =>
  symbol.getDeclarations().map(decl => decl.getStartLineNumber()).reduce((currentMin, current) => Math.min(currentMin, current), Infinity)

export const getExported = (file: SourceFile) =>
  file.getExportSymbols()
    .filter(symbol => !mustIgnore(symbol, file))
    .map(symbol => ({
      name: symbol.compilerSymbol.name,
      line: lineNumber(symbol)
    }));
export const getExportedSymbols = (file: SourceFile) =>
  file.getExportSymbols()
    .filter(symbol => !mustIgnore(symbol, file));

/* Returns all the "import './y';" imports, which must be for side effects */
export const importsForSideEffects = (file: SourceFile): IAnalysedResult[] =>
  file
    .getImportDeclarations()
    .map(decl => ({
      moduleSourceFile: getModuleSourceFile(decl),
      definitelyUsed: isDefinitelyUsedImport(decl)
    }))
    .filter(meta => meta.definitelyUsed && !!meta.moduleSourceFile)
    .map(({ moduleSourceFile }) => ({
      file: moduleSourceFile,
      symbols: [],
      type: AnalysisResultTypeEnum.DEFINITELY_USED
    }));

const exportWildCards = (file: SourceFile): IAnalysedResult[] =>
  file
    .getExportDeclarations()
    .filter(decl => decl.getText().includes("*"))
    .map((decl) => ({
      file: getModuleSourceFile(decl),
      symbols: [],
      type: AnalysisResultTypeEnum.DEFINITELY_USED
    }));

const getDefinitelyUsed = (file: SourceFile): IAnalysedResult[] => ([
  ...importsForSideEffects(file),
  ...exportWildCards(file),
]);

const getReferences = (
  originalList: SourceFileReferencingNodes[],
  skipper?: RegExp
): SourceFileReferencingNodes[] => {
  if (skipper) {
    return originalList.filter(file =>
      !skipper.test(file.getSourceFile().compilerNode.fileName)
    );
  }
  return originalList;
}
export const getPotentiallyUnused = (project: Project, file: SourceFile, skipper?: RegExp): IAnalysedResult => {
  const ls = project.getLanguageService();
  const program = ls.getProgram();
  const sourceFiles = project.getSourceFiles().map(s => s.compilerNode);
  const exportedSyms = getExportedSymbols(file);
  const referencedInFile = new Set<string>();
  const referenced = new Set<string>();
  const checker = program.getTypeChecker().compilerObject;
  const sourceFilesSet = new real_ts.Set(sourceFiles.map(function (f) { return f.fileName; }));
  Exp:
  for(const exp of exportedSyms) {
    let inFile = false;
    const decl = exp.getDeclarations()[0];
    const declStart = decl.getStart();
    const declEnd = decl.getEnd();
    const ContinueExp = {};
    try {
      real_ts.FindAllReferences.Core.getReferencedSymbolsForSymbol(
        exp.compilerSymbol as any as real_ts.Symbol,
        undefined,
        sourceFiles as any as real_ts.SourceFile[],
        sourceFilesSet as any as real_ts.ReadonlySet<string>,
        checker as any as real_ts.TypeChecker,
        {
          isCancellationRequested() { return false },
          throwIfCancellationRequested() {}
        },
        {
          implementations: false,
          use: real_ts.FindAllReferences.FindReferencesUse.Other
        },
        __callback__
      );
    } catch(e) {
      if(e === ContinueExp) continue Exp;
      throw e;
    }
    function __callback__(entry: real_ts.FindAllReferences.NodeEntry) {
      const referenceNode = entry.node;
      const referenceSourceFile = referenceNode.getSourceFile();
      if(skipper?.test(referenceSourceFile.fileName)) return;
      if(referenceSourceFile !== file.compilerNode as any as real_ts.SourceFile) {
        // TODO switch to ts.FindAllReferences.Core.eachExportReference?
        referenced.add(exp.compilerSymbol.name);
        throw ContinueExp;
      } else if(!inFile && (referenceNode.getStart() < declStart || referenceNode.getEnd() > declEnd)) {
        inFile = true;
        // TODO Switch to ts.FindAllReferences.Core.isSymbolReferencedInFile?
        referencedInFile.add(exp.compilerSymbol.name);
      } else {
      }
    }
    // const refs = ls.findReferences(decl);
    // for(const ref of refs) {
    //   for(const ref2 of ref.getReferences()) {
    //     const sf = ref2.getSourceFile();
    //     if(skipper?.test(sf.compilerNode.fileName)) continue;
    //     if(sf !== file) {
    //       // TODO switch to ts.FindAllReferences.Core.eachExportReference?
    //       referenced.add(exp.compilerSymbol.name);
    //       continue Exp;
    //       // console.log(exp.getName(), 'referenced by', ref2.getSourceFile().getFilePath());
    //     } else if(!inFile && (ref2.getTextSpan().getStart() < decl.getStart() || ref2.getTextSpan().getEnd() > decl.getEnd())) {
    //       // console.log(decl.getText(), 'referenced by', file.getFullText().slice(ref2.getTextSpan().getStart(), ref2.getTextSpan().getEnd()));
    //       inFile = true;
    //       // TODO Switch to ts.FindAllReferences.Core.isSymbolReferencedInFile?
    //       referencedInFile.add(exp.compilerSymbol.name);
    //     }
    //   }
    // }
  }
  // const exported = getExported(file);

  // const idsInFile = file.getDescendantsOfKind(ts.SyntaxKind.Identifier);
  // const referenceCounts = countBy(x => x)((idsInFile || []).map(node => node.getText()));
  // const referencedInFile = Object.entries(referenceCounts)
  //   .reduce(
  //     (previous, [name, count]) => previous.concat(count > 1 ? [name] : []),
  //     []
  //   );

  const unused = referenced.has("*") ? [] :
    exportedSyms.filter(exp => !referenced.has(exp.compilerSymbol.name))
      .map(exp => ({ name: exp.compilerSymbol.name,
        line: lineNumber(exp), usedInModule: referencedInFile.has(exp.compilerSymbol.name) }))

  return {
    file: file.getFilePath(),
    symbols: unused,
    type: AnalysisResultTypeEnum.POTENTIALLY_UNUSED
  };
};

const emitTsConfigEntrypoints = (entrypoints: string[], onResult: OnResultType) =>
  entrypoints.map(file => ({
    file,
    symbols: [],
    type: AnalysisResultTypeEnum.DEFINITELY_USED,
  })).forEach(emittable => onResult(emittable))

const filterSkippedFiles = (sourceFiles: SourceFile[], skipper: RegExp | undefined) => {
  if (!skipper) {
    return sourceFiles;
  }

  return sourceFiles.filter(file => !skipper.test(file.getSourceFile().compilerNode.fileName));
}

export const analyze = (project: Project, onResult: OnResultType, entrypoints: string[], skipPattern?: string) => {
  const ls = project.getLanguageService();
  const skipper = skipPattern ? new RegExp(skipPattern) : undefined;

  const sourceFiles = project.getSourceFiles();
  filterSkippedFiles(project.getSourceFiles(), skipper)
  .forEach(file => {
    [
      getPotentiallyUnused(project, file, skipper),
      ...getDefinitelyUsed(file),
    ].forEach(result => {
      if (!result.file) return // Prevent passing along a "null" filepath. Fixes #105
      onResult({ ...result, file: realpathSync(result.file) })
    });
  });

  emitTsConfigEntrypoints(entrypoints, onResult);
};

export function unused() {

}

/**
 * see unused
 * @see {unused}
 */
export function other() {

}